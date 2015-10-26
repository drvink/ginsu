{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}
module Gale.KeyCache(
    dumpKey,
    KeyCache,
    newKeyCache,
    getKey,
    getPKey,
    parseKey,
    keyIsPubKey,
    keyIsPrivKey,
    keyIsPublic,
    putKey,
    noKey,
    numberKeys,
    keyRequestPuff,
    keyToPkey,
    getPubKeyBytes,
    keyResponsePuff

    ) where

import Atom
import Data.Bits
import Data.Char
import Control.Concurrent
import qualified Control.Exception as E
import System.Directory
import EIO
import ErrorLog
import Gale.Proto(decodeTime, decodeFrags, xdrReadUInt, getGaleDir, decodeFragments)
import GenUtil
import Data.List
import Control.Monad
import PackedString
import Gale.Puff
import RSA
import System.Mem.Weak
import Data.Word
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Text.ParserCombinators.ReadP.ByteString
import Data.Binary.Get(runGet,Get())

stons :: [Char] -> [Word8]
stons = map (fromIntegral . ord)

private_magic1, private_magic2, private_magic3 :: BS.ByteString
pubkey_magic1, pubkey_magic2 :: BS.ByteString

private_magic1 = BS.pack $ stons "h\DC3\000\001"
private_magic2 = BS.pack $ stons "h\DC3\000\003"
private_magic3 = BS.pack $ stons "GALE\000\002"


pubkey_magic1 = BS.pack $ stons "h\DC3\000\000"
pubkey_magic2 = BS.pack $ stons "h\DC3\000\002"
pubkey_magic3 = BS.pack $ stons "GALE\000\001"

galeRSAModulusBits = 1024
galeRSAModulusLen = (galeRSAModulusBits + 7) `div` 8
galeRSAPrimeBits = (galeRSAModulusBits + 1) `div` 2
galeRSAPrimeLen = (galeRSAPrimeBits + 7) `div` 8


data KeyCache = KeyCache {
    pkeyCache :: !(MVar (Map.Map String (Weak (Key,EvpPkey)))),
    kkeyCache :: !(MVar (Map.Map String (Weak Key))),
    pubkeyCache :: !(MVar (Map.Map String (Weak BS.ByteString))),
    galeDir :: String
    }

numberKeys kc = fmap Map.size $ readMVar (kkeyCache kc)

keyIsPubKey :: HasFragmentList a => a -> Bool
keyIsPubKey k = hasFragment k f_rsaExponent
keyIsPrivKey :: HasFragmentList a => a -> Bool
keyIsPrivKey k =  (hasFragment k f_rsaPrivateExponent)
keyIsPublic :: HasFragmentList a => a -> Bool
keyIsPublic k = any nullPS $ getFragmentStrings k f_keyMember

newKeyCache galeDir = do
    pk <- newMVar Map.empty
    kk <- newMVar Map.empty
    pbk <- newMVar Map.empty
    return KeyCache { {- keyCache = kc, publicKeyCache = pkc,-} galeDir = galeDir, pkeyCache = pk, kkeyCache = kk, pubkeyCache = pbk }

keyToRSAElems :: Monad m => Key -> m (RSAElems BS.ByteString)
keyToRSAElems fl = do
    if not (keyIsPubKey fl) then fail "key does not have bits" else do
    n <- getFragmentData fl f_rsaModulus
    e <- getFragmentData fl f_rsaExponent
    if not (keyIsPrivKey fl) then
        return RSAElemsPublic { rsaN = n, rsaE = e } else do
    d <- getFragmentData fl f_rsaPrivateExponent
    iqmp <- getFragmentData fl f_rsaPrivateCoefficient
    pq <- getFragmentData fl f_rsaPrivatePrime
    dmpq1 <- getFragmentData fl f_rsaPrivatePrimeExponent
    let (p,q) = BS.splitAt galeRSAPrimeLen pq     -- should be "rsa.bits"?
        (dmp1,dmq1) = BS.splitAt galeRSAPrimeLen dmpq1
    return RSAElemsPrivate {
        rsaN = n,
        rsaE = e,
        rsaD = d ,
        rsaIQMP = iqmp,
        rsaP = p,
        rsaQ =  q,
        rsaDMP1 = dmp1,
        rsaDMQ1 = dmq1
        }

{-
keyToRSAElems :: Monad m => Key -> m (RSAElems [Word8])
keyToRSAElems fl = do
    if not (keyIsPubKey fl) then fail "key does not have bits" else do
    n <- getFragmentData fl f_rsaModulus
    e <- getFragmentData fl f_rsaExponent
    if not (keyIsPrivKey fl) then
        return RSAElemsPublic { rsaN = BS.unpack n, rsaE = BS.unpack e } else do
    d <- getFragmentData fl f_rsaPrivateExponent
    iqmp <- getFragmentData fl f_rsaPrivateCoefficient
    pq <- getFragmentData fl f_rsaPrivatePrime
    dmpq1 <- getFragmentData fl f_rsaPrivatePrimeExponent
    let (p,q) = splitAt galeRSAPrimeLen (BS.unpack pq)     -- should be "rsa.bits"?
        (dmp1,dmq1) = splitAt galeRSAPrimeLen (BS.unpack dmpq1)
    return RSAElemsPrivate {
        rsaN = BS.unpack n,
        rsaE = BS.unpack e,
        rsaD = BS.unpack d ,
        rsaIQMP = BS.unpack iqmp,
        rsaP = p,
        rsaQ =  q,
        rsaDMP1 = dmp1,
        rsaDMQ1 = dmq1
        }
        -}



keyToPkey :: Key -> IO EvpPkey
keyToPkey key  = do
    re <- keyToRSAElems key
    putLog LogDebug $ show re

    createPkey re

getPKey :: KeyCache -> String -> IO (Maybe (Key,EvpPkey))
getPKey kc kn =  modifyMVar (pkeyCache kc) f where
    f pkeyCache | Just v <- Map.lookup kn pkeyCache = do
        v <- deRefWeak v
        case v of
            Just _ -> return (pkeyCache, v)
            Nothing -> g pkeyCache
    f pkeyCache = g pkeyCache
    g pkeyCache = do
        k <- getKey kc kn
        case k of
            Nothing -> return (pkeyCache, Nothing)
            Just v | not (hasFragment v f_rsaModulus) ->  return (pkeyCache, Nothing)
            Just key -> do
                --rsa <- keyToRSA v
                --pkey <- pkeyNewRSA rsa
                pkey <- keyToPkey key
                let kp =  (key,pkey)
                ptr <- mkWeakPtr kp Nothing
                return (Map.insert  kn ptr pkeyCache, Just kp)

blankKey kn = (Key kn [])

noKey :: KeyCache -> String -> IO ()
noKey kc kn = modifyMVar (kkeyCache kc) f where
    f kkeyCache | Just _ <- Map.lookup kn kkeyCache = return (kkeyCache, ())
    f kkeyCache = do
        n <- mkWeakPtr (blankKey kn) Nothing
        return (Map.insert kn n  kkeyCache, ())


putKey :: KeyCache -> BS.ByteString -> IO ()
putKey kc xs = do
    let mk = parseKey $ BS.unpack xs
    case mk of
        Nothing -> return ()
        Just v'@(Key kn _) -> do
            modifyMVar (kkeyCache kc) f where
                f kkeyCache | Just _ <- Map.lookup kn kkeyCache = return (kkeyCache, ())
                f kkeyCache = do
                    E.catch (createDirectory $ galeDir kc ++ "/auth/") (\(_ :: E.IOException) -> return ())
                    E.catch (createDirectory $ galeDir kc ++ "/auth/cache/") (\(_ :: E.IOException) -> return ())
                    --xs <- unsafeThaw xs
                    --bnds <- getBounds xs
                    atomicWrite  (galeDir kc ++ "/auth/cache/" ++ kn ++ ".gpub")  $
                        \h -> BS.hPut h xs -- hPutArray h xs (rangeSize bnds)
                    v' <- mkWeakPtr v' Nothing
                    return (Map.insert kn v' kkeyCache, ())

-- XXX duplication; the key cache should store a ByteString of the pubkey too so
-- that we don't need this separate function
getPubKeyBytes :: KeyCache -> String -> IO (Maybe BS.ByteString)
getPubKeyBytes kc kn = modifyMVar (pubkeyCache kc) f where
    f pkc | Just v <- Map.lookup kn pkc = do
      v' <- deRefWeak v
      case v' of
        Just _ -> return (pkc, v')
        Nothing -> g pkc
    f pkc = g pkc
    g pkc = do
      v <- tryIO getFromDisk
      case v of
        Left _ -> return (pkc, Nothing)
        Right v' -> do
          v'' <- mkWeakPtr v' Nothing
          return (Map.insert kn v'' pkc, Just v')
    getFromDisk = do
        let pp = galeDir kc ++ "/auth/private/"
            knames = pp ++ kn ++ ".gpub"
            gf fn = readRawFile fn >>= \xs -> do
              let pk = BS.pack xs
                  k = parseKey xs
              return (fn, pk, k)
        xs <- tryIO $ gf knames
        case xs of
          Left _ -> lose
          Right (_,bs,mk) | Just _ <- mk -> return bs
          Right _ -> lose
      where
        lose = ioError $ userError "bad key"

getKey :: KeyCache -> String -> IO (Maybe Key)
getKey kc kn = modifyMVar (kkeyCache kc) f where
    f kkeyCache | Just v <- Map.lookup kn kkeyCache = do
        v <- deRefWeak v
        case v of
            Just _ -> return (kkeyCache, v)
            Nothing -> g kkeyCache
    f kkeyCache = g kkeyCache
    g kkeyCache = do
            v <- tryIO getFromDisk
            case v of
                Left _ -> return (kkeyCache, Nothing)
                Right v' -> do
                    v'' <- mkWeakPtr v' Nothing
                    return (Map.insert kn v'' kkeyCache, Just v')
    getFromDisk :: IO Key
    getFromDisk = do
        let pc = galeDir kc ++ "/auth/cache/"
            pp = galeDir kc ++ "/auth/private/"
            knames = [ pp ++ kn ++ ".gpri", pp ++ kn ++ ".gpub", pp ++ kn , pc ++ kn ++ ".gpub" ]
            gf fn = readRawFile fn >>= return . (,) fn . parseKey
        --putLog LogDebug $ "Looking for: " ++ show  knames
        xs <- tryMapM gf knames
        let ks = [ k | (_,Just k@(Key n _)) <- xs, kn == n]
        let nk = foldr (\(Key _ fl) (Key n fl') -> Key n (fl `mergeFrags` fl')) (Key kn []) ks
        --(key_c, fn) <- (first  gn)
        --key <- parseKey key_c
        --rsa <- pubKeyToRSA key
        --pkey <- pkeyNewRSA rsa
        if null (getFragmentList nk) then ioError $ userError "bad key" else return nk

flipLocalPart :: String -> String
flipLocalPart s | '@' `notElem` s = s
flipLocalPart s = nbp ++ ep where
    nbp = concat $ reverse (groupBy f bp)
    (bp,ep) = span (/= '@') s
    f '.' '.' = True
    f x y | x /= '.' && y /= '.' = True
    f _ _ = False

keyRequestPuff :: String -> Puff
keyRequestPuff s = emptyPuff { cats = [Category ("_gale.query." ++ n, d)], fragments = [(f_questionKey, FragmentText $ packString s), (f_questionKey',FragmentText $ packString s')]} where
    s' = flipLocalPart s
    Category (n,d) = catParseNew s

keyResponsePuff :: Maybe BS.ByteString -> String -> Puff
keyResponsePuff kb s = emptyPuff { cats = [Category ("_gale.key." ++ n, d)], fragments = [resp]} where
    resp = case kb of
      Just x -> (f_answerKey', FragmentData x)
      Nothing -> (f_answerKeyError', FragmentText $ packString $ "cannot find " ++ s)
    Category (n,d) = catParseNew s

--la xs = listArray (0, length xs - 1) xs
--la xs = BS.pack xs

parseWord32 :: ReadP Word32
parseWord32 = do
    b1 <- get
    b2 <- get
    b3 <- get
    b4 <- get
    return $ (fromIntegral b4) .|. (fromIntegral b3 `shiftL` 8) .|.
            (fromIntegral b2 `shiftL` 16) .|. (fromIntegral b1 `shiftL` 24)

parseWord16 :: ReadP Word16
parseWord16 = do
    b1 <- get
    b2 <- get
    return $ (fromIntegral b2) .|. (fromIntegral b1 `shiftL` 8)

parseIntegral32 = fmap fromIntegral parseWord32


keyDecode12 = do
    bits <- parseIntegral32
    modulusD <- splitRLE galeRSAModulusLen
    exponentD <- splitRLE galeRSAModulusLen
    privateExponentD <- splitRLE (galeRSAPrimeLen * 2)
    privatePrimeD <- splitRLE galeRSAModulusLen
    privatePrimeExponentD <- splitRLE (galeRSAPrimeLen * 2)
    privateCoefficientD <- splitRLE galeRSAPrimeLen
    let fl = [("rsa.modulus",FragmentData  modulusD),
          ("rsa.exponent",FragmentData exponentD),
          ("rsa.private.exponent",FragmentData  privateExponentD),
          ("rsa.private.prime",FragmentData  privatePrimeD),
          ("rsa.private.prime.exponent",FragmentData privatePrimeExponentD),
          ("rsa.private.coefficient",FragmentData  privateCoefficientD),
          ("rsa.bits", FragmentInt (fromIntegral bits))
         ]
    return [ (fromString x,y) | (x,y) <- fl]
    {-
    (bits,ys) = xdrReadUInt xs
    (modulusD,xs') = splitRLE galeRSAModulusLen ys
    (exponentD,xs'') = splitRLE galeRSAModulusLen xs'
    (privateExponentD,xs''') = splitRLE (galeRSAPrimeLen * 2) xs''
    (privatePrimeD,xs'''') = splitRLE galeRSAModulusLen xs'''
    (privatePrimeExponentD,xs''''') = splitRLE (galeRSAPrimeLen * 2) xs''''
    (privateCoefficientD,_) = splitRLE galeRSAPrimeLen xs'''''

-}

eof = do
    l <- look
    guard $ BS.null l

parseNullString :: ReadP String
parseNullString = map (chr . fromIntegral) `fmap` manyTill get (char 0)

parseLenString :: ReadP String
parseLenString = do
    len <- parseIntegral32
    ws <- replicateM len parseWord16
    return $ map (chr . fromIntegral) ws



keyParse :: ReadP Key
keyParse = choice [ppk1,ppk2,ppk3,pk1,pk2,pk3] where
    ppk1 = do
        string private_magic1
        kn <- fmap flipLocalPart parseNullString
        fl <- keyDecode12
        return $ Key kn fl
    ppk2 = do
        string private_magic2
        kn <- fmap flipLocalPart parseLenString
        fl <- keyDecode12
        return $ Key kn fl
    ppk3 = do
        string private_magic3
        kn <- fmap flipLocalPart parseLenString
        fl <- toParser decodeFrags
        return $ Key kn fl
    pk1 = do
        string pubkey_magic1
        kn <- fmap flipLocalPart parseNullString
        (eof >> return (Key kn [])) +++ do
        comment <- parseNullString
        fl <- parsePublic12
        skipMany get -- the signature
        return $ Key kn ([(f_keyOwner, FragmentText (packString comment))] ++ fl)
    pk2 = do
        string pubkey_magic2
        kn <- fmap flipLocalPart parseLenString
        (eof >> return (Key kn [])) +++ do
        comment <- parseLenString
        fl' <- parsePublic12
        let fl = ((f_keyOwner, FragmentText (packString comment)):fl')
        (eof >> return (Key kn fl)) +++ do
        ts <- replicateM 16 get
        te <- replicateM 16 get
        skipMany get
        --_signature <- parseRest
        return $ Key kn $ fl ++ [(f_keySigned,FragmentTime (decodeTime ts)), (f_keyExpires,FragmentTime (decodeTime te))]
    pk3 = do
        string pubkey_magic3
        kn <- fmap flipLocalPart parseLenString
        fl <- toParser decodeFrags
        return $ Key kn fl
    parsePublic12 = do
        bits <- parseIntegral32
        --modulus <- (MkP (\x -> Just (splitRLE galeRSAModulusLen x)))
        --exponent <- (MkP (\x -> Just (splitRLE galeRSAModulusLen x)))
        modulus <- splitRLE galeRSAModulusLen
        exponent <- splitRLE galeRSAModulusLen
        return [(f_rsaModulus, FragmentData modulus),
                (f_rsaExponent, FragmentData exponent),
                (f_rsaBits, FragmentInt $ fromIntegral bits)]


parser :: Show a => ReadP a -> BS.ByteString -> Maybe a
parser p bs = case readP_to_S p bs of
    x:xs ->
        let (a,bs) = if null xs then x else last xs in
        if BS.null bs then Just a else Nothing
    _ -> Nothing

parseKey :: [Word8] -> Maybe Key
parseKey xs = do
    (Key kn fl) <- parser keyParse (BS.pack xs)
    fl <- unsignFragments fl
    return $ Key kn fl

splitRLE :: Int -> ReadP BS.ByteString
splitRLE n = f n [] where
    f n _ | n < 0 = fail "invalid RLE encoding"
    f 0 xs = return $ BS.concat (reverse xs)
    f n rs = plain +++ rep where
        plain = do
            c <- get
            guard $ c .&. 0x80 /= 0
            let count = fromIntegral $ (c .&. 0x7f) + 1
            (x,_) <- gather (skip count)
            f (n - count) (x:rs)
        rep = do
            c <- get
            guard $ c .&. 0x80 == 0
            x <- get
            let count = fromIntegral $ (c .&. 0x7f) + 1
            f (n - count) (BS.replicate count x:rs)

{-

splitRLE :: Int -> [Word8] -> ([Word8], [Word8])
splitRLE n _ | n < 0 = error "invalid RLE encoding"
splitRLE 0 xs = ([],xs)
splitRLE n (c:xs) | c .&. 0x80 /= 0 = (take count xs ++ ys,rest) where
    count = fromIntegral $ (c .&. 0x7f) + 1
    (ys,rest) = splitRLE (n - count) (drop count xs)
splitRLE n (c:x:xs) | c .&. 0x80 == 0 = (replicate count x ++ ys,rest) where
    count = fromIntegral $ (c .&. 0x7f) + 1
    (ys,rest) = splitRLE (n - count) xs
splitRLE _ _  = error "invalid RLE encoding"

-}

rest = do
    s <- look
    skip (BS.length s)
    return s

toParser :: Get a -> ReadP a
toParser g = (runGet g . LBS.fromChunks . (:[])) `fmap` rest

{-
unsignData :: Monad m => BS.ByteString -> m (FragmentList,Key)
unsignData xs = flip parser xs $ do
    l <- parseIntegral32
    skip 4
    sb <- parseIntegral32

-}



unsignData :: [Word8] -> Maybe (FragmentList,Key)
unsignData xs = do
    key <- parseKey $ take (fromIntegral (l - (8 + sb))) (drop (fromIntegral sb) xs'')
    return (fl,key) where
        (l,xs') = xdrReadUInt xs
        (sb,xs'') = xdrReadUInt (drop 4 xs')
        fl = (decodeFragments $ drop (fromIntegral l + 4) xs')

unsignFragments :: FragmentList -> Maybe FragmentList
unsignFragments tfl | (xs:_) <- [xs | (n,FragmentData xs) <- tfl, n == f_securitySignature] = do
    (fl,_) <- unsignData (BS.unpack xs)
    unsignFragments fl
unsignFragments x = return x

----------------
-- test code
----------------

{-# NOTINLINE dumpKey #-}
dumpKey :: String -> IO ()
dumpKey arg = do
    gd <- getGaleDir
    kc <- newKeyCache gd
    nk <- getKey kc arg
    key <- case nk of
        Just key -> return key
        Nothing -> do
            c <- readRawFile arg
            maybe (fail "parseKey") return $ parseKey c
    putStrLn $ showKey key



{-

getPrivateKey :: KeyCache -> String -> IO (Maybe (Key,EvpPkey))
getPrivateKey gc kn = modifyMVar (keyCache gc) f where
    f kc = case [x|x@(Key kn' _,_) <- kc, kn == kn'] of
        (v:_) -> return (kc,Just v)
        [] -> do
            v <- ioMp getFromDisk
            return ((maybeToList v ++ kc),v)
    getFromDisk = do
        let gd = galeDir gc
        let p = (gd ++ "/auth/private/")
            knames = [p ++ kn, p ++ kn ++ ".gpri"]
            gn = (map (\fn -> (readRawFile fn >>= \c -> return (c,fn))) knames)
        (key_c, fn) <- (first  gn)
        key <- parseKey key_c
        rsa <- privkeyToRSA key
        pkey <- pkeyNewRSA rsa
        putLog LogNotice $ "Retrieved private key from disk: " ++ fn
        return (key,pkey)



getPublicKey :: KeyCache -> String -> IO (Maybe (Key,EvpPkey))
getPublicKey gc kn = modifyMVar (publicKeyCache gc) f where
    f keyCache | Just v <- lookupFM keyCache kn = return (keyCache, Just v)
    f keyCache = do
            v <- ioM getFromDisk
            return $ case v of
                Nothing -> (keyCache, Nothing)
                Just v' -> (addToFM keyCache kn v', Just v')
    getFromDisk :: IO (Key,EvpPkey)
    getFromDisk = do
        let gd = galeDir gc
        let p = (gd ++ "/auth/cache/")
            knames = [ p ++ kn ++ ".gpub"]
            gn = (map (\fn -> (readRawFile fn >>= \c -> return (c,fn))) knames)
        putLog LogDebug $ "Looking for: " ++ show  knames
        (key_c, fn) <- (first  gn)
        key <- parseKey key_c
        rsa <- pubKeyToRSA key
        pkey <- pkeyNewRSA rsa
        return (key,pkey)
-}

{-
fragmentData' x y = fragmentData (packString x) y
privkeyToRSA :: Key -> IO RSA
privkeyToRSA (Key _ fl)  = do
    rsa <- rsaNew
    fragmentData' "rsa.modulus" fl >>= bn_bin2bn >>= rsaSetN rsa
    fragmentData' "rsa.exponent" fl >>= bn_bin2bn >>= rsaSetE rsa
    fragmentData' "rsa.private.exponent" fl >>= bn_bin2bn >>= rsaSetD rsa
    fragmentData' "rsa.private.coefficient" fl >>= bn_bin2bn >>= rsaSetIQMP rsa
    xs <- fragmentData' "rsa.private.prime" fl
    let (p,q) = splitAt galeRSAPrimeLen xs
    bn_bin2bn p >>= rsaSetP rsa
    bn_bin2bn q >>= rsaSetQ rsa
    xs <- fragmentData' "rsa.private.prime.exponent" fl
    let (dmp1,dmq1) = splitAt galeRSAPrimeLen xs
    bn_bin2bn dmp1 >>= rsaSetDMP1 rsa
    bn_bin2bn dmq1 >>= rsaSetDMQ1 rsa
    rsaCheckKey rsa
    return rsa

pubKeyToRSA :: Key -> IO RSA
pubKeyToRSA (Key _ fl)  = do
    rsa <- rsaNew
    fragmentData' "rsa.modulus" fl >>= bn_bin2bn >>= rsaSetN rsa
    fragmentData' "rsa.exponent" fl >>= bn_bin2bn >>= rsaSetE rsa
    --fragmentData' "rsa.private.exponent" fl >>= bn_bin2bn >>= rsaSetD rsa
    --fragmentData' "rsa.private.coefficient" fl >>= bn_bin2bn >>= rsaSetIQMP rsa
    --xs <- fragmentData' "rsa.private.prime" fl
    --let (p,q) = splitAt galeRSAPrimeLen xs
    --bn_bin2bn p >>= rsaSetP rsa
    --bn_bin2bn q >>= rsaSetQ rsa
    --xs <- fragmentData' "rsa.private.prime.exponent" fl
    --let (dmp1,dmq1) = splitAt galeRSAPrimeLen xs
    --bn_bin2bn dmp1 >>= rsaSetDMP1 rsa
    --bn_bin2bn dmq1 >>= rsaSetDMQ1 rsa
    --rsaCheckKey rsa
    return rsa

-}
{-
keyToRSA :: Key -> IO RSA
keyToRSA (Key _ fl)  = do
    rsa <- rsaNew
    fragmentData' "rsa.modulus" fl >>= bn_bin2bn >>= rsaSetN rsa
    fragmentData' "rsa.exponent" fl >>= bn_bin2bn >>= rsaSetE rsa
    if  (hasFragment fl (packString "rsa.private.exponent")) then  do
        fragmentData' "rsa.private.exponent" fl >>= bn_bin2bn >>= rsaSetD rsa
        fragmentData' "rsa.private.coefficient" fl >>= bn_bin2bn >>= rsaSetIQMP rsa
        xs <- fragmentData' "rsa.private.prime" fl
        let (p,q) = splitAt galeRSAPrimeLen xs     -- should be "rsa.bits"?
        bn_bin2bn p >>= rsaSetP rsa
        bn_bin2bn q >>= rsaSetQ rsa
        xs <- fragmentData' "rsa.private.prime.exponent" fl
        let (dmp1,dmq1) = splitAt galeRSAPrimeLen xs
        bn_bin2bn dmp1 >>= rsaSetDMP1 rsa
        bn_bin2bn dmq1 >>= rsaSetDMQ1 rsa
        rsaCheckKey rsa
      else do
        return ()

       -- rsaSetD rsa nullPtr
       -- rsaSetIQMP rsa nullPtr
       -- rsaSetP rsa nullPtr
       -- rsaSetQ rsa nullPtr
       -- rsaSetDMP1 rsa nullPtr
       -- rsaSetDMQ1 rsa nullPtr
    return rsa
-}
