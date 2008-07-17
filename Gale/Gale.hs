{-# OPTIONS_GHC -fallow-overlapping-instances #-}
module Gale.Gale(
    GaleContext,
    galeNextPuff,
    reconnectGaleContext,
    connectionStatus,
    galeSendPuff,
    hostStrings,
    galeWillPuff,
    withGale,
    galeSetProxys,
    galeAddCategories,
    verifyDestinations,
    gCategory,
    keyCache,
    getGaleDir) where


import Char(chr,ord)
import IO hiding(bracket, bracket_)
import List
import Maybe
import System.Time
import Time

import Control.Concurrent
import Control.Exception
import Control.Exception as E
import Data.Bits
import Network.BSD
import Network.Socket
import PackedString
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS

import Atom
import Control.Monad.Error
import Data.Array.IO
import Data.Monoid
import Doc.DocLike((<+>))
import EIO
import ErrorLog
import Gale.Proto
import Gale.KeyCache
import Gale.Puff
import GenUtil hiding(replicateM)
import qualified System.Posix as Posix
import RSA
import SimpleParser

-- TODO - prove concurrent-correctness, make sure all network errors are accounted for.

-------------------
-- Gale Constants
-------------------

galePort :: PortNumber
galePort = 11512
hostStrings s = [s, "gale." ++ s, s ++ ".gale.org."]


type PuffStatus = ()

data GaleContext = GaleContext {
    connectionStatus :: !(MVar (Either String String)),
    channel :: !(Chan Puff),
    proxy :: !(MVar [String]),
    gThread :: ThreadId,
    gHandle :: !(MVar Handle),
    gCategory :: !(MVar [Category]),
    keyCache :: !KeyCache
    }

void a = a >> return ()




-----------------
-- Implementation
-----------------

withGale :: [String] -> (GaleContext -> IO a) -> IO a
withGale ps io = withSocketsDo $ do
    Posix.installHandler Posix.sigPIPE Posix.Ignore Nothing
    bracket (newGaleContext ps []) destroyGaleContext io
    --gc <- newGaleContext ps []
    --r- <- io gc
    --destroyGaleContext gc
    --return r

newGaleContext ps cs = do
    let ncs = map catParseNew cs
    cats <- newMVar $ ncs
    c <- newChan
    ps <- return (case ps of [] -> snub (concatMap (hostStrings . categoryCell) ncs); _ -> ps)
    status <- newMVar $ Left $ "Attempting to connect to: " ++ unwords ps
    pmv <- newMVar ps
    hv <- newEmptyMVar
    --keycachev <- newMVar []
    --pkcache <- newMVar emptyFM
    galeDir <- getGaleDir
    keyCache <- newKeyCache galeDir
    let gc = GaleContext { connectionStatus = status, gThread = undefined, gHandle = hv, gCategory = cats, channel = c, proxy = pmv, keyCache = keyCache {- keyCache = keycachev, publicKeyCache = pkcache -} }
    thd <- forkIO (connectThread gc ps hv)
    sendGimme gc
    return gc { gThread = thd }

galeAddCategories :: GaleContext -> [Category] -> IO ()
galeAddCategories gc cs = do
    action <- modifyMVar (gCategory gc) $ \cs' ->
        let ncs = snub (cs ++ cs') in
         if ncs == cs' then return (ncs,return ()) else return (ncs,sendGimme gc)
    action
    --sendGimme gc

galeSetProxys :: GaleContext -> [String] -> IO ()
galeSetProxys gc ps = do
    modifyMVar_ (proxy gc) $ \_ -> return (snub ps)
    sendGimme gc

sendGimme :: GaleContext -> IO ()
sendGimme gc = void $ forkIO $ do
    withMVar (gHandle gc) $ \h -> do
        ncs <- readMVar $ gCategory gc
        putLog LogDebug $ "sendGimme:" ++ (show $ ncs)
        let gs = concatInter ":" (map catShowOld ncs)
        putWord32 h 2
        putWord32 h (fromIntegral $ length gs * 2)
        putRaw h $ galeEncodeString gs
        hFlush h

destroyGaleContext gc = killThread $ gThread gc

connectTo hostname port = do
    --proto <- getProtocolNumber "tcp"
    bracketOnError
	(socket AF_INET Stream 6)
	(sClose)  -- only done if there's an error
        (\sock -> do
      	  he <- getHostByName hostname
      	  connect sock (SockAddrInet port (hostAddress he))
      	  socketToHandle sock ReadWriteMode
	)



attemptConnect s = do
    h <- connectTo s galePort
    --hSetBuffering h NoBuffering
    return (h,s)

spc [] = []
spc s = v : spc (drop 1 r) where
    (v,r) = span (/= ':') s

emptyPuffer :: MVar Handle -> IO ()
emptyPuffer hv = repeatM_ (threadDelay 30000000 >> sendEmptyPuff) where
	sendEmptyPuff = withMVar hv $ \h -> do
		putWord32 h 0
		putWord32 h 8
		putWord32 h 0
		putWord32 h 0
		hFlush h

connectThread :: GaleContext ->  [String] -> MVar Handle -> IO ()
connectThread gc _ hv = retry 5.0 ("ConnectionError") doit where
    openHandle = do
        ds <- readMVar $ proxy gc
        swapMVar (connectionStatus gc)  $ Left $ "Attempting to connect to: " ++ unwords ds
        trySeveral (map attemptConnect ds)
    doit = bracket openHandle (hClose . fst) $ \(h,hn) -> do
	putWord32 h 1
	_ <- readWord32 h  -- version
        sendGimme gc
        swapMVar (connectionStatus gc) $ Right hn
	bracket_ (putMVar hv h) (takeMVar hv) $
	  bracket (forkIO (emptyPuffer hv)) killThread $ \_ -> repeatM_ $ do
	    w <- readWord32 h
	    l <- readWord32 h
            bs <- LBS.hGet h (fromIntegral l)
	    when (w == 0) $ do
                let hash = sha1 bs
                let (cat,puff) = runGet decodePuff bs
                cat <- tryMapM parseCategoryOld (spc $ cat)
                ct <- getClockTime
                let ef = \xs -> ((fromString "_ginsu.timestamp",FragmentTime ct):(fromString "_ginsu.spumbuster", FragmentText (packString (bsToHex hash))):xs)
                p' <- galeDecryptPuff gc Puff { signature = [], cats = cat, fragments = ef puff}
                writeChan (channel gc) $ p'
                case getFragmentData p' f_answerKey' of
                    Just d -> putKey (keyCache gc) d
                    Nothing -> return ()
                case (cats p',getFragmentString p' f_answerKeyError') of
                    ([Category (n,d)],Just _) | "_gale.key." `isPrefixOf` n -> noKey (keyCache gc) (catShowNew $ Category (drop 10 n,d))
                    (_,_) -> return ()


decodePuff :: Get (String,FragmentList)
decodePuff = do
    clen <- getWord32be
    cs <- replicateM (fromIntegral (clen `div` 2)) getWord16be
    --getWord32be
    skip 4
    fl <- decodeFrags
    return (map (chr . fromIntegral) cs, fl)

decodeFrags :: Get FragmentList
decodeFrags = df [] where
    df xs = do
        b <- isEmpty
        if b then return (reverse xs) else do
        z <- decodeFrag
        df (z:xs)

decodeFrag :: Get (Atom,Fragment)
decodeFrag = do
    ty <- getWord32be
    ln <- getWord32be
    fnl <- getWord32be
    fn <- replicateM (fromIntegral fnl) getWord16be
    let fn' = fromString (map (chr . fromIntegral) fn)
    let dl = fromIntegral $ ln - (4 + (fnl * 2))
    fr <- case ty of
        0 -> do
            tx <- replicateM (dl `div` 2) getWord16be
            return $ FragmentText $ packString (map (chr . fromIntegral) tx)
        1 -> do
            --d <- bytes dl
            --d <- replicateM dl byte
            up <- getBytes dl
            return $ FragmentData up
        2 -> do
            w <- getWord64be
            skip 8 --word64
            return $ FragmentTime (TOD (fromIntegral w) 0)
        3 -> do
            w <- getWord32be
            return $ FragmentInt (fromIntegral w)
        4 -> do
            up <- getBytes dl
            return $ FragmentNest (runGet decodeFrags (LBS.fromChunks [up]))
        _ -> fail $ "unknown fragment type: " <+> show ty <+> show ln <+> show fnl <+> show fn'
    return (fn', fr)

galeNextPuff :: GaleContext -> IO Puff
galeNextPuff gc = do
    p <- readChan $ channel gc
    --p' <- galeDecryptPuff gc p
    --case getFragmentData p' f_answerKey' of
    --    Just d -> putKey (keyCache gc) d
    --    Nothing -> return ()
    putLog LogDebug $ "Puff gotten: \n" ++ (indent 4 $ showPuff p)
    return p

reconnectGaleContext gc = do
    -- p <- readMVar $ proxy gc
    void $ forkIO $ attempt $ readMVar (gHandle gc) >>= hClose


galeSendPuff :: GaleContext -> Puff -> IO PuffStatus
galeSendPuff gc puff = void $ forkIO $ do
    putLog LogInfo $ "sending puff:\n" ++ (indent 4 $ showPuff puff)
    puff' <- expandEncryptionList gc puff
    writeChan (channel gc) puff'
    d <- createPuff  gc False puff'
    retry 3.0 "error sending puff" $ withMVar (gHandle gc) $ \h -> putRaw h d >> hFlush h

galeWillPuff :: GaleContext -> Puff -> IO ()
galeWillPuff gc puff = void $ forkIO $ do
    putLog LogDebug $ "willing puff:\n" ++ (indent 4 $ showPuff puff)
    d <- createPuff gc True puff
    retry 3.0 "error sending puff" $ withMVar (gHandle gc) $ \h -> putRaw h d >> hFlush h


getPrivateKey kc kn = getPKey kc kn >>= \n -> case n of
    Just (k,_) | not $ keyIsPrivKey k -> return Nothing
    o -> return o

collectSigs :: [Signature] -> ([String],[String])
collectSigs ss = liftT2 (snub, snub) $ cs ss ([],[]) where
    cs ((Unverifyable _):_) _ = error "attempt to create unverifyable puff"
    cs (Signed (Key k _):ss) (ks,es) = cs ss (k:ks,es)
    cs (Encrypted es':ss) (ks,es) = cs ss (ks,es' ++ es)
    cs [] x = x

createPuff :: GaleContext -> Bool -> Puff -> IO [Word8]
createPuff _ will puff | [] <- signature puff = do
    let cn = galeEncodeString (concatInter ":" (map catShowOld $ cats puff))
    let ad = xdrWriteUInt (fromIntegral $ length cn) (cn ++ xdrWriteUInt 0 []) ++ createFragments (fragments puff)
    let pd = xdrWriteUInt (if will then 1 else 0) (xdrWriteUInt (fromIntegral $ length ad) ad)
    evaluate pd
createPuff gc will p | (kn:_,es) <-  collectSigs (signature p) = do
    getPrivateKey (keyCache gc) kn >>= \v -> case v of
	Nothing -> createPuff gc will $ p {signature = []}
	Just (Key _ kfl,pkey) -> do
	    sfl <- case fragmentString f_keyOwner kfl of
		Just o -> return [(f_messageSender, FragmentText o)]
		Nothing -> return []
	    let fl = xdrWriteUInt 0 (createFragments (fragments p `mergeFrags` sfl))
	    sig <- signAll pkey (BS.pack fl)
	    let sd = signature_magic1 ++ xdrWriteUInt (fromIntegral $ BS.length sig) [] ++ BS.unpack sig ++ pubkey_magic3 ++ xdrWriteUInt (fromIntegral $ length kn) [] ++ galeEncodeString kn
                fd = xdrWriteUInt (fromIntegral $ length sd) $ sd ++ fl
                fragments = [(f_securitySignature,FragmentData (BS.pack fd))]
            nfragments <- cryptFragments gc es fragments
	    createPuff gc will $ p {signature = [], fragments = nfragments }
createPuff _ _ _ = error "createPuff: invalid arguments"

cryptFragments :: GaleContext -> [String] -> FragmentList -> IO FragmentList
cryptFragments _ [] fl = return fl
cryptFragments gc ss fl = do
    putLog LogDebug $ "cryptFragments " ++ show ss
    ks <-  mapM (getPKey (keyCache gc)) ss
    let ks' = [ (n,x) | Just (Key n _,x) <- ks]
        fl' = xdrWriteUInt 0 (createFragments fl)
        n = fromIntegral (length ks')
    --putStrLn $ show (ks,ks',fl')
    (d,ks,iv) <- encryptAll (snds ks') (BS.pack fl')
    --putLog LogDebug $ show (d,ks,iv)
    --return [(f_securityEncryption,FragmentData $ la (cipher_magic2 ++ BS.unpack iv ++ xdrWriteUInt n (foldr f (BS.unpack d) $ zip (fsts ks') (map BS.unpack ks)) ))]
    return [(f_securityEncryption,FragmentData $ (bs_cipher_magic2 `BS.append` iv `BS.append` BS.pack (xdrWriteUInt n (foldr f (BS.unpack d) $ zip (fsts ks') (map BS.unpack ks)) )))]
  where
     f (kn,kd) x = xdrWriteUInt (fromIntegral $ length kn) $ galeEncodeString kn ++ xdrWriteUInt (fromIntegral $ length kd) (kd ++ x)



--keyIsPublic key = any nullPS (getFragmentStrings key f_keyMember)

expandEncryptionList :: GaleContext -> Puff -> IO Puff
expandEncryptionList gc p = do
    ks <- fmap (normalizeDest . mconcat) $ mapM (findDest gc ) (cats p)
    case ks of
        DestPublic -> return p
        DestUnknown _ -> return p
        DestEncrypted ks -> return p { signature = Encrypted [ n | k@(Key n _) <- ks, keyIsPubKey k ]: signature p }

--    if any (maybe True (keyIsPublic ) ) (snds ks) then return p else do
--        return p { signature = Encrypted [ n | Just k@(Key n _) <-  snds ks, keyIsPubKey k ]: signature p }

putGaleString s = putByteString . BS.pack $ galeEncodeString s

putFragments :: FragmentList -> Put
putFragments fl = mapM_ f fl where
    f (s',f) = putWord32be t >> putWord32be (fromIntegral $ LBS.length nxs) >> putLazyByteString nxs  where
        nxs = runPut (n >> xs)
	(t, xs) = g f
	n = putWord32be (fromIntegral $ length s) >> (putGaleString s)
        s = toString s'
    g (FragmentData ws) = (1, putByteString ws)
    g (FragmentText s) = (0, putGaleString (unpackPS s))
    g (FragmentTime (TOD s _)) = (2, putWord64be (fromIntegral s) >> putWord64be 0)
    g (FragmentInt i) = (3, putWord32be (fromIntegral i))
    g (FragmentNest fl) = (4, putFragments fl)

createFragments :: FragmentList -> [Word8]
createFragments fl = concatMap f fl where
    f (s',f) = xdrWriteUInt t $ xdrWriteUInt (fromIntegral $ length n + length xs) $ n ++ xs where
	(t, xs) = g f
	n = xdrWriteUInt (fromIntegral $ length s) (galeEncodeString s)
        s = toString s'
    g (FragmentData ws) = (1, BS.unpack ws)
    g (FragmentText s) = (0, galeEncodeString (unpackPS s))
    g (FragmentTime (TOD s _)) = (2, replicate 4 0 ++ xdrWriteUInt (fromIntegral s) (replicate 8 0))
    g (FragmentInt i) = (3, xdrWriteUInt (fromIntegral i) [])
    g (FragmentNest fl) = (4, createFragments fl)



catfixes = [ ("/", ".|"), (".", "/"), (":", "..") ]

parseCategoryOld :: Monad m => String -> m Category
parseCategoryOld = parser p where
    con cs | [nv] <- [x ++ (con $ drop (length y) cs) |(x,y) <- catfixes, y `isPrefixOf` cs] = nv
    con (c:cs) = c:con cs
    con "" = ""
    bl [] = []
    bl [_] = []
    bl (x:xs) = x:bl xs
    p = do
	char '@'
	d <- many (noneOf "/")
	parseExact "/user/"
	c <- parseRest
	return (Category (con (bl c),d))

catShowOld :: Category -> String
catShowOld (Category (c,d)) = "@" ++ d ++ "/user/" ++ con c ++ "/" where
    con cs | [nv] <- [x ++ (con $ drop (length y) cs) |(y,x) <- catfixes, y `isPrefixOf` cs] = nv
    con (c:cs) = c:con cs
    con "" = ""


--------------------
-- Security routines
--------------------

galeDecryptPuff :: GaleContext -> Puff -> IO Puff
galeDecryptPuff gc p | (Just xs) <- getFragmentData p (f_securitySignature) = tryElse p $ do
    let (l,xs') = xdrReadUInt (BS.unpack xs)
	(sb,xs'') = xdrReadUInt (drop 4 xs')
	fl = (decodeFragments $ drop (fromIntegral l + 4) xs') ++ [f|f <- fragments p, fst f /= f_securitySignature]
    key <- parseKey $ take (fromIntegral (l - (8 + sb))) (drop (fromIntegral sb) xs'')
    galeDecryptPuff gc $ p {signature = (Unverifyable key): signature p, fragments = fl}
galeDecryptPuff gc p | (Just xs) <- getFragmentData p f_securityEncryption = tryElse p $ do
    (cd,ks) <- parser pe (BS.unpack xs)
    dfl <- first (map (td' (BS.pack cd)) [ (BS.pack x,y,BS.pack z) | (x,y,z) <- ks])
    let dfl' = dfl ++ [f|f <- fragments p, fst f /= f_securityEncryption]
    galeDecryptPuff gc $ p {signature = (Encrypted (map (\(_,n,_) -> n) ks)): signature p, fragments = dfl'}  where
	pe = (parseExact cipher_magic1 >> pr parseNullString) <|> (parseExact cipher_magic2 >> pr parseLenString)
	pk pkname iv = do
	    kname <- pkname
	    keydata <- parseLenData
	    return $ (iv,kname,keydata)
	pr pkname = do
	    iv <- parseSome 8
	    keycount <-  parseIntegral32
	    ks <- replicateM keycount (pk pkname iv)
	    xs <- parseRest
	    return (xs,ks)
	td' cd (iv,kname,keydata) = do
	    Just (_,pkey) <- getPrivateKey (keyCache gc) kname
	    dd <- decryptAll keydata iv pkey cd
	    --let dfl = decodeFragments (drop 4 (BS.unpack dd))
	    return $ runGet decodeFrags (LBS.fromChunks [BS.drop 4 dd])
galeDecryptPuff _ x = return x


--data DestinationStatus = DSPublic { dsComment :: String } | DSPrivate { dsComment :: String } | DSGroup { dsComment :: String, dsComponents :: [DestinationStatus] } | DSUnknown

--verifyDestinations :: [Category] -> [(Category,DestinationStatus)]
--verifyDestinations cs = [ (c,DSUnknown) | c <- cs ]



verifyDestinations' :: GaleContext -> [Category] -> IO [(Category, String)]
verifyDestinations' gc cs = mapM dc cs where
    dc c | categoryIsSystem c = return (c,"Special location (puff will not be encrypted)")
    dc c = dc' c >>= return . (,) c
    dc' c = do
        ks <- findDest gc c
        case ks of
            DestPublic -> return "Public category (puff will not be encrypted)"
            DestEncrypted _ -> return "Private category"
            -- DestUnknown _ | Just x <- nextTry (fst c) -> dc' (x,snd c)
            DestUnknown _ -> return "Unknown destination (puff will not be encrypted)"
--    nextTry "*" = fail "no more"
--    nextTry ss = return $ reverse (nt (reverse ss)) where
--        nt ('*':'.':ss)  = nt ss
--        nt ss =  '*' : dropWhile (/= '.') ss


{-
        if any isNothing (snds ks) then
            if fst c == "*" then
                return "*UNKNOWN*  (puff will not be encrypted)"
             else
                dc' (nextTry (fst c), snd c)
         else
          pp [ (x,y) | (x,Just y) <- ks]

    pp ks | any isPublic (snds ks) = return "Public Category (puff will not be encrypted)"
    pp _ = return "Private Category"
    isPublic key = any nullPS (getFragmentStrings key f_keyMember)

fetchKeymembers :: GaleContext -> String -> IO [(String,Maybe Key)]
fetchKeymembers gc s = do
    km <- fk [s] []
    putLog LogNotice $ "fetchKeymembers " ++ s ++ "\n" ++ show km
    return km
   where
    fk [] xs = return xs      -- we are done
    fk ("":_) xs = return xs  -- public category
    fk (s:ss) xs | s `elem` fsts xs = fk ss xs
    fk (s:ss) xs = getPublicKey (keyCache gc) s >>= maybe (fk ss ((s,Nothing):xs)) (r . fst) where
        r :: Key -> IO [(String,Maybe Key)]
        r k = fk (map unpackPS (getFragmentStrings k f_keyMember) ++ ss) ((s,Just k):xs)

fetchKeymembers :: GaleContext -> String -> IO [(String,Maybe Key)]
fetchKeymembers _ s | "_gale." `isPrefixOf` s = return [(s,Just $ emptyKey s)]
fetchKeymembers gc s = do
    km <- fk [s] []
    putLog LogNotice $ "fetchKeymembers: " ++ s ++  show km
    return km
   where
    fk [] xs = return xs      -- we are done
    fk ("":_) _ = return [(s,Just $ emptyKey s)]  -- public category
    fk (s:ss) xs | s `elem` fsts xs = fk ss xs
    fk (s:ss) xs = getKey (keyCache gc) s >>= maybe (fk ss ((s,Nothing):xs)) r where
        r :: Key -> IO [(String,Maybe Key)]
        r k = fk (map unpackPS (getFragmentStrings k f_keyMember) ++ ss) ((s,Just k):xs)
-}

data Dest = DestPublic | DestUnknown [String] | DestEncrypted [Key]
    deriving(Eq,Show)

instance Monoid Dest where
    mempty = DestEncrypted []
    DestUnknown a `mappend` DestUnknown b = DestUnknown (a ++ b)
    DestUnknown a `mappend` _ = DestUnknown a
    _ `mappend` DestUnknown a  = DestUnknown a
    DestPublic `mappend` _ = DestPublic
    _ `mappend` DestPublic = DestPublic
    DestEncrypted a `mappend` DestEncrypted b = DestEncrypted (a ++ b)

normalizeDest DestPublic = DestPublic
normalizeDest (DestUnknown xs) = DestUnknown $ snub xs
normalizeDest (DestEncrypted xs) = DestEncrypted $ snub xs

fetchKeys :: GaleContext -> String -> IO Dest
fetchKeys _ s | "_gale." `isPrefixOf` s = return DestPublic
fetchKeys _ s | "_gale@" `isPrefixOf` s = return DestPublic
fetchKeys gc s = do
    km <- fk [s] []
    putLog LogDebug $ "fetchKeys: " ++ s ++  show km
    return $ normalizeDest (mconcat $ snds km) where
        fk [] xs = return xs      -- we are done
        fk ("":_) _ = return [("",DestPublic)]
        fk (s:ss) xs | s `elem` fsts xs = fk ss xs
        fk (s:ss) xs = getKey (keyCache gc) s >>= maybe (requestKey gc (catParseNew s) >> fk ss ((s,DestUnknown [s]):xs)) r where
            r (Key _ []) = fk ss ((s,DestUnknown [s]):xs)
            r k = fk (map unpackPS (getFragmentStrings k f_keyMember) ++ ss) ((s,DestEncrypted [k]):xs)

categoryIsSystem (Category (n,_)) | "_gale." `isPrefixOf` n = True
categoryIsSystem (Category (n,_)) | "_gale" == n = True
categoryIsSystem _ = False


requestKey _ c | categoryIsSystem c = return ()
requestKey gc c = do
    let c' = catShowNew c
    v <- getKey (keyCache gc) c'
    when (isNothing v) $ do
        galeAddCategories gc [Category ("_gale.key", categoryCell c)]
        d <- createPuff  gc False $ keyRequestPuff c'
        putLog LogDebug $ "sending request for: " ++ c'
        retry 3.0 "error sending puff" $ withMVar (gHandle gc) $ \h -> putRaw h d >> hFlush h


findDest gc c = fd c >>= res where
    -- cn = catShowNew c
    fd c = do
        ks <- fetchKeys gc (catShowNew c)
        case ks of
                DestUnknown _ | Category (a,b) <- c, Just x <- nextTry a -> fd (Category (x,b))
                k -> return k
    res x = case x of
        DestUnknown _ -> do
--            let cs = map catParseNew ss
--            galeAddCategories gc (("_gale.key", snd c):[("_gale.key", x) | x <- snds cs])
--            let f Nothing = []
--                f (Just x) = x:f (nextTry x)
--                ac = snub $ concat $ map (flip (,) (snd c)) (f $ Just $ fst c) : [ map (flip (,) d)  (f $ Just n) | (n,d) <- cs]
--                g x = requestKey gc x
--            putLog LogDebug $ "attempting to lookup: " ++ show ac
--            --mapM_ g (f $ Just $ fst c)
--            mapM_ g ac
            threadDelay 1000000  -- try again after one second
            fd c
        k -> return k






nextTry "*" = fail "no more"
nextTry ss = return $ reverse (nt (reverse ss)) where
    nt ('*':'.':ss)  = nt ss
    nt ss =  '*' : dropWhile (/= '.') ss


verifyDestinations :: GaleContext -> [Category] -> IO String

verifyDestinations _ [] = return "** No Destinations **"
verifyDestinations gc cs = do
    (ds) <- verifyDestinations' gc cs
    let --x = "DestinationStatus: " ++ d
        xs = map f ds
        f (c,x) =  (catShowNew c) ++ ": " ++ x
    return (unlines (xs))





----------------------
-- Gale stream Parsing
----------------------

xdrWriteUInt :: Word32 -> [Word8] -> [Word8]
xdrWriteUInt x bs = (b1:b2:b3:b4:bs) where
    b1 = fromIntegral $ (x `shiftR` 24) .&. 0xFF
    b2 = fromIntegral $ (x `shiftR` 16) .&. 0xFF
    b3 = fromIntegral $ (x `shiftR` 8) .&. 0xFF
    b4 = fromIntegral $ x .&. 0xFF

putWord32 :: Handle -> Word32 -> IO ()
putWord32 h x = do
    hPutChar h $ chr $ fromIntegral $ (x `shiftR` 24)
    hPutChar h $ chr $ fromIntegral $ (x `shiftR` 16) .&. 0xFF
    hPutChar h $ chr $ fromIntegral $ (x `shiftR` 8) .&. 0xFF
    hPutChar h $ chr $ fromIntegral $ x .&. 0xFF

readWord32 :: Handle -> IO Word32
readWord32 h = do
    a <- newArray_ (0,3)
    n <- hGetArray h a 4
    when (n /= 4) $ fail "short read."
    [b1,b2,b3,b4] <- getElems a
    return $ (fromIntegral b4) .|. (fromIntegral b3 `shiftL` 8) .|.
	     (fromIntegral b2 `shiftL` 16) .|. (fromIntegral b1 `shiftL` 24)

galeEncodeString :: String -> [Word8]
galeEncodeString cs = concatMap (f . ord) (concat $ map (\c -> if c == '\n' then "\r\n" else [c]) cs) where
    f x = (b1:b2:[]) where
	b1 = fromIntegral $ (x `shiftR` 8) .&. 0xFF
	b2 = fromIntegral $ x .&. 0xFF


--------------
-- Key Parsing
--------------

stons :: [Char] -> [Word8]
stons = map (fromIntegral . ord)

cipher_magic1, cipher_magic2 :: [Word8]
signature_magic1 :: [Word8]

cipher_magic1 = stons "h\DC3\002\000"
cipher_magic2 = stons "h\DC3\002\001"

bs_cipher_magic1 = BS.pack cipher_magic1
bs_cipher_magic2 = BS.pack cipher_magic2

signature_magic1 = stons "h\DC3\001\000"


