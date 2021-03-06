{-# LANGUAGE OverlappingInstances, FlexibleInstances, PatternGuards, ScopedTypeVariables #-}
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


import Data.Char(chr,ord)
import System.IO
import Data.List
import Data.Maybe
import System.Time

import Control.Concurrent
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM.TChan (TChan(..), newTChanIO, readTChan, writeTChan)
import Control.Exception as E
import Control.Monad.STM (atomically)
import Data.Bits
import Network.BSD
import Network.Socket
import PackedString
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Control.Applicative ((<|>))

import Atom
import Control.Monad.Error (when, replicateM)
import Data.Array.IO
import Data.Monoid
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
    channel :: !(TChan Puff),
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
    c <- newTChanIO
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
        (close)  -- only done if there's an error
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
connectThread gc _ hv = retryIO 5.0 ("ConnectionError") doit where
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
                    (catl,puff) = runGet decodePuff bs
                    cat = mapMaybe parseCategoryOld (spc catl)
                ct <- getClockTime
                let ef = \xs -> ((fromString "_ginsu.timestamp",FragmentTime ct):(fromString "_ginsu.spumbuster", FragmentText (packString (bsToHex hash))):xs)
                p' <- galeDecryptPuff gc Puff { signature = [], cats = cat, fragments = ef puff}
                case [(kh, k, data_, sig)
                     | RequestingKey kh k data_ sig <- signature p'] of
                  [] -> finishPuff p'
                  (kh, k, data_, sig):_ -> void $ forkIO $ do
                    let encs = [s | s@(Encrypted _) <- signature p']
                        unver = p' { signature = Unverifyable k:encs }
                    dest <- wait kh
                    np <- case dest of
                      DestEncrypted (k':_) -> do
                        mkey <- verifySignature k' data_ sig
                        return $ p' { signature = mkey:encs }
                      _ -> return unver
                    finishPuff np
              where
                finishPuff np = do
                  atomically $ writeTChan (channel gc) np
                  case getFragmentData np f_answerKey' of
                      Just d -> putKey (keyCache gc) d
                      Nothing -> return ()
                  case (cats np,getFragmentString np f_answerKeyError') of
                      ([Category (n,d)],Just _) | "_gale.key." `isPrefixOf` n -> noKey (keyCache gc) (catShowNew $ Category (drop 10 n,d))
                      (_,_) -> return ()
                  maybeReplyToKeyQuery gc np

maybeReplyToKeyQuery :: GaleContext -> Puff -> IO ()
maybeReplyToKeyQuery gc p | Just kn <- getFragmentString p f_questionKey = do
  let kn' = unpackPS kn
  mkb <- getPubKeyBytes (keyCache gc) kn'
  d <- createPuff gc False $ keyResponsePuff mkb kn'
  putLog LogDebug $ "sending reply for: " ++ kn'
  retryIO 3.0 "error sending puff" $ withMVar (gHandle gc) $ \h -> LBS.hPut h d >> hFlush h
maybeReplyToKeyQuery _ _ = return ()

decodePuff :: Get (String,FragmentList)
decodePuff = do
    clen <- getWord32be
    cs <- replicateM (fromIntegral (clen `div` 2)) getWord16be
    --getWord32be
    skip 4
    fl <- decodeFrags
    return (map (chr . fromIntegral) cs, fl)


galeNextPuff :: GaleContext -> IO Puff
galeNextPuff gc = do
    p <- atomically $ readTChan $ channel gc
    --p' <- galeDecryptPuff gc p
    --case getFragmentData p' f_answerKey' of
    --    Just d -> putKey (keyCache gc) d
    --    Nothing -> return ()
    putLog LogDebug $ "Puff gotten: \n" ++ (indent 4 $ showPuff p)
    return p

reconnectGaleContext gc = do
    -- p <- readMVar $ proxy gc
    void $ forkIO $ attemptIO $ readMVar (gHandle gc) >>= hClose


galeSendPuff :: GaleContext -> Puff -> IO PuffStatus
galeSendPuff gc puff = void $ forkIO $ do
    putLog LogInfo $ "sending puff:\n" ++ (indent 4 $ showPuff puff)
    puff' <- expandEncryptionList gc puff
    atomically $ writeTChan (channel gc) puff'
    d <- createPuff  gc False puff'
    retryIO 3.0 "error sending puff" $ withMVar (gHandle gc) $ \h -> LBS.hPut h d >> hFlush h

galeWillPuff :: GaleContext -> Puff -> IO ()
galeWillPuff gc puff = void $ forkIO $ do
    putLog LogDebug $ "willing puff:\n" ++ (indent 4 $ showPuff puff)
    d <- createPuff gc True puff
    retryIO 3.0 "error sending puff" $ withMVar (gHandle gc) $ \h -> LBS.hPut h d >> hFlush h


getPrivateKey kc kn = getPKey kc kn >>= \n -> case n of
    Just (k,_) | not $ keyIsPrivKey k -> return Nothing
    o -> return o

collectSigs :: [Signature] -> ([String],[String])
collectSigs ss = liftT2 (snub, snub) $ cs ss ([],[]) where
    cs sig x@(ks,es) = case sig of
      (RequestingKey _ _ _ _):_ -> lose
      (Unverifyable _):_ -> lose
      (Signed (Key k _)):ss -> cs ss (k:ks,es)
      (Encrypted es':ss) -> cs ss (ks,es' ++ es)
      [] -> x
    lose = error "attempt to create unverifiable puff"

tagWord :: Int -> Put -> Put
tagWord i p = putWord32be (fromIntegral i) >> p

class RawPut a where
    rawPut :: a -> Put

instance RawPut BS.ByteString where
    rawPut = putByteString

instance RawPut LBS.ByteString where
    rawPut = putLazyByteString

instance RawPut [Word8] where
    rawPut = putByteString . BS.pack

runPutBS = BS.concat . LBS.toChunks . runPut

createPuff :: GaleContext -> Bool -> Puff -> IO LBS.ByteString
createPuff _ will puff | [] <- signature puff = do
    let cn = runPut $ putGaleString (concatInter ":" (map catShowOld $ cats puff))
    let ad = runPut $ tagWord (fromIntegral $ LBS.length cn) $ do putLazyByteString cn; tagWord 0 $ putFragments (fragments puff)
    let pd = tagWord (if will then 1 else 0) (tagWord (fromIntegral $ LBS.length ad) (putLazyByteString ad))
    evaluate $ runPut pd
createPuff gc will p | (kn:_,es) <-  collectSigs (signature p) = do
    getPrivateKey (keyCache gc) kn >>= \v -> case v of
        Nothing -> createPuff gc will $ p {signature = []}
        Just (Key _ kfl,pkey) -> do
            sfl <- case fragmentString f_keyOwner kfl of
                Just o -> return [(f_messageSender, FragmentText o)]
                Nothing -> return []
            let fl = runPutBS $ tagWord 0 (putFragments (fragments p `mergeFrags` sfl))
            sig <- signAll pkey fl
            let sd = runPutBS $ do
                    putByteString bs_signature_magic1
                    tagWord (BS.length sig) (putByteString sig)
                    putByteString (BS.pack pubkey_magic3)
                    tagWord (fromIntegral $ length kn) (putGaleString kn)
                fd = tagWord (BS.length sd) (putByteString sd) >> putByteString fl
                fragments = [(f_securitySignature,FragmentData (runPutBS fd))]
            nfragments <- cryptFragments gc es fragments
            createPuff gc will $ p {signature = [], fragments = nfragments }
createPuff _ _ _ = error "createPuff: invalid arguments"

cryptFragments :: GaleContext -> [String] -> FragmentList -> IO FragmentList
cryptFragments _ [] fl = return fl
cryptFragments gc ss fl = do
    putLog LogDebug $ "cryptFragments " ++ show ss
    ks <-  mapM (getPKey (keyCache gc)) ss
    let ks' = [ (n,x) | Just (Key n _,x) <- ks]
        fl' = putWord32be 0 >> (putFragments fl)
        n = fromIntegral (length ks')
    --putStrLn $ show (ks,ks',fl')
    (d,ks,iv) <- encryptAll (snds ks') (BS.concat (LBS.toChunks $ runPut fl'))
    return [(f_securityEncryption,FragmentData . BS.concat $ [bs_cipher_magic2,iv] ++ LBS.toChunks (runPut $ do putWord32be n;  mapM_ g (zip (fsts ks') ks) ; putByteString d))]
  where
     g (kn,kd) = do
        putWord32be (fromIntegral $ length kn)
        putGaleString kn
        putWord32be (fromIntegral $ BS.length kd)
        putByteString kd



expandEncryptionList :: GaleContext -> Puff -> IO Puff
expandEncryptionList gc p = do
    ks <- fmap (normalizeDest . mconcat) $ mapM (findDest gc ) (cats p)
    case ks of
        DestPublic -> return p
        DestUnknown _ -> return p
        DestEncrypted ks -> return p { signature = Encrypted [ n | k@(Key n _) <- ks, keyIsPubKey k ]: signature p }

--    if any (maybe True (keyIsPublic ) ) (snds ks) then return p else do
--        return p { signature = Encrypted [ n | Just k@(Key n _) <-  snds ks, keyIsPubKey k ]: signature p }

putGaleString :: String -> Put
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



catfixes = [ ("/", ".|"), (".", "/"), (":", "..") ]

parseCategoryOld :: String -> Maybe Category
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

verifySignature :: Key -> BS.ByteString -> BS.ByteString -> IO Signature
verifySignature k data_ sig = do
  pkey <- keyToPkey k
  rv <- verifyAll pkey data_ sig
  return $ (if rv then Signed else Unverifyable) k

galeDecryptPuff :: GaleContext -> Puff -> IO Puff
galeDecryptPuff gc p = handle (\(_ :: IOException) -> return p) $ galeDecryptPuff' gc p
galeDecryptPuff' gc p | (Just xs) <- getFragmentData p f_securitySignature = do
    let (l,xs') = xdrReadUInt (BS.unpack xs)
        (sb,xs'') = xdrReadUInt (drop 4 xs')
        fl = (decodeFragments $ drop (fromIntegral l + 4) xs') ++ [f | f <- fragments p, fst f /= f_securitySignature]
        sigoff = 12 -- skip length hdr (4), sig magic (4), sig len (4)
        siglen = fromIntegral sb
        keylen = fromIntegral l - (8 + siglen)
        keyoff = sigoff + siglen -- key directly follows sig
        sig = BS.take siglen $ BS.drop sigoff xs
        data_ = BS.drop (keyoff + keylen) xs
    key <- maybe (fail "parseKey") return $ parseKey $ take keylen (drop siglen xs'')
    let (Key kn _) = key
    kgc <- getKey (keyCache gc) kn
    mkey <- case kgc of
      Nothing -> do
        let senderc = catParseNew kn
        h <- async (findDest gc senderc)
        return $ RequestingKey h key data_ sig
      Just k -> verifySignature k data_ sig
    galeDecryptPuff' gc $ p {signature = mkey: signature p, fragments = fl}
galeDecryptPuff' gc p | (Just xs) <- getFragmentData p f_securityEncryption = do
    (cd,ks) <- maybe (fail "parseKey") return $ parser pe (BS.unpack xs)
    dfl <- firstIO (map (td' (BS.pack cd)) [ (BS.pack x,y,BS.pack z) | (x,y,z) <- ks])
    let dfl' = dfl ++ [f | f <- fragments p, fst f /= f_securityEncryption]
    galeDecryptPuff' gc $ p {signature = (Encrypted (map (\(_,n,_) -> n) ks)): signature p, fragments = dfl'}  where
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
galeDecryptPuff' _ x = return x


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
        retryIO 3.0 "error sending puff" $ withMVar (gHandle gc) $ \h -> LBS.hPut h d >> hFlush h


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
bs_signature_magic1 :: BS.ByteString

cipher_magic1 = stons "h\DC3\002\000"
cipher_magic2 = stons "h\DC3\002\001"

bs_cipher_magic1 = BS.pack cipher_magic1
bs_cipher_magic2 = BS.pack cipher_magic2

bs_signature_magic1 = BS.pack $ stons "h\DC3\001\000"


