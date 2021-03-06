{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, CPP #-}
module RSA(
    EvpPkey,
    decryptAll,
    encryptAll,
    signAll,
    createPkey,
    sha1,
    bsToHex,
    RSAElems(..),
    verifyAll
    ) where

#include "my_rsa.h"

import Foreign.C
import Control.Exception as E
import ErrorLog
import Foreign
import Numeric(showHex)

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

data RSAStruct
data EVP_PKEY
data EVP_CIPHER
data EVP_CIPHER_CTX
data EVP_MD_CTX
data EVP_MD
data BIGNUM

type RSA = Ptr RSAStruct

type EvpPkey = ForeignPtr EVP_PKEY

fi x = fromIntegral x

------------------------
-- marshalling utilities
------------------------

throwZero_ s = throwIf_ (== 0) (const s)


withData :: BS.ByteString -> (Ptr CUChar -> CInt -> IO a) -> IO a
withData xs f = BS.unsafeUseAsCStringLen xs (\ (cp,cl) -> f (castPtr cp) (fromIntegral cl))

returnData :: Int -> (Ptr CUChar -> Ptr CInt -> IO z) -> IO BS.ByteString
returnData sz f = do
        alloca $ \bp ->
            allocaArray sz $ \m -> do
                f m bp
                s <- peek bp
                BS.packCStringLen (castPtr m, fromIntegral s)

---------------
-- RSA routines
---------------


foreign import ccall unsafe "my_rsa.h RSA_new" rsaNew :: IO RSA
-- foreign import ccall unsafe "my_rsa.h RSA_free" rsaFree :: RSA -> IO ()

foreign import ccall unsafe "my_rsa.h RSA_check_key" rsa_check_key :: RSA -> IO CInt

rsaCheckKey :: RSA -> IO ()
rsaCheckKey rsa = throwZero_ "RSA_check_key" $ rsa_check_key rsa

data RSAElems a = RSAElemsPrivate {
    rsaN :: a,
    rsaE :: a,
    rsaD :: a,
    rsaIQMP :: a,
    rsaP :: a,
    rsaQ :: a,
    rsaDMP1 :: a,
    rsaDMQ1 :: a
    } |  RSAElemsPublic { rsaN :: a, rsaE :: a }
    deriving(Show)



----------------------
-- Envelope Encryption
----------------------

foreign import ccall unsafe "openssl/evp.h EVP_DigestInit" evpSignInit :: Ptr EVP_MD_CTX -> Ptr EVP_MD -> IO ()
foreign import ccall unsafe "openssl/evp.h EVP_DigestUpdate" evpSignUpdate :: Ptr EVP_MD_CTX -> Ptr CUChar -> CInt -> IO ()
foreign import ccall unsafe "openssl/evp.h EVP_SignFinal" evpSignFinal :: Ptr EVP_MD_CTX -> Ptr CUChar -> Ptr CInt -> Ptr EVP_PKEY -> IO CInt

foreign import ccall unsafe "openssl/evp.h EVP_OpenInit" evpOpenInit :: Ptr EVP_CIPHER_CTX -> Ptr EVP_CIPHER -> Ptr CUChar -> CInt -> Ptr CUChar -> Ptr EVP_PKEY -> IO CInt
foreign import ccall unsafe "openssl/evp.h EVP_DecryptUpdate" evpOpenUpdate :: Ptr EVP_CIPHER_CTX -> Ptr CUChar -> Ptr CInt -> Ptr CUChar -> CInt -> IO CInt
foreign import ccall unsafe "openssl/evp.h EVP_OpenFinal" evpOpenFinal :: Ptr EVP_CIPHER_CTX -> Ptr CUChar -> Ptr CInt -> IO CInt


foreign import ccall unsafe "openssl/evp.h EVP_SealInit" evpSealInit :: Ptr EVP_CIPHER_CTX -> Ptr EVP_CIPHER -> Ptr (Ptr CUChar) -> Ptr CInt -> Ptr CUChar -> Ptr (Ptr EVP_PKEY) -> CInt -> IO CInt
foreign import ccall unsafe "openssl/evp.h EVP_EncryptUpdate" evpSealUpdate :: Ptr EVP_CIPHER_CTX -> Ptr CUChar -> Ptr CInt -> Ptr CUChar -> CInt -> IO CInt
foreign import ccall unsafe "openssl/evp.h EVP_SealFinal" evpSealFinal :: Ptr EVP_CIPHER_CTX -> Ptr CUChar -> Ptr CInt -> IO CInt

foreign import ccall unsafe "openssl/evp.h EVP_des_ede3_cbc" evpDesEde3Cbc :: IO (Ptr EVP_CIPHER)
foreign import ccall unsafe "openssl/evp.h EVP_md5" evpMD5 :: IO (Ptr EVP_MD)

foreign import ccall unsafe "openssl/evp.h EVP_DigestInit" evpDigestInit :: Ptr EVP_MD_CTX -> Ptr EVP_MD -> IO CInt
foreign import ccall unsafe "openssl/evp.h EVP_DigestUpdate" evpDigestUpdate :: Ptr EVP_MD_CTX -> Ptr a -> CInt -> IO CInt
foreign import ccall unsafe "openssl/evp.h EVP_VerifyFinal" evpVerifyFinal :: Ptr EVP_MD_CTX -> Ptr CUChar -> CInt -> Ptr EVP_PKEY -> IO CInt

foreign import ccall unsafe evpCipherContextBlockSize :: Ptr EVP_CIPHER_CTX -> IO Int


evp_OpenUpdate :: Ptr EVP_CIPHER_CTX -> BS.ByteString -> IO BS.ByteString
evp_OpenUpdate cctx ind = do
    bsz <- evpCipherContextBlockSize cctx
    let sz = BS.length ind + bsz
    withData ind (\ina inl -> returnData sz (\outa outl -> throwZero_ "EVP_OpenUpdate" (evpOpenUpdate cctx outa outl ina inl)))

evp_OpenFinal ::  Ptr EVP_CIPHER_CTX -> IO BS.ByteString
evp_OpenFinal cctx = do
    bsz <- evpCipherContextBlockSize cctx
    d <- returnData bsz (\outa outl -> throwZero_ "EVP_OpenFinal" (evpOpenFinal cctx outa outl))
    return d



foreign import ccall unsafe
#if OPENSSL_VERSION_NUMBER < 0x10100000L
  "EVP_CIPHER_CTX_create"
#else
  "EVP_CIPHER_CTX_new"
#endif
  evpCipherCtxNew :: IO (Ptr EVP_CIPHER_CTX)

foreign import ccall unsafe
#if OPENSSL_VERSION_NUMBER < 0x10100000L
  "EVP_CIPHER_CTX_destroy"
#else
  "EVP_CIPHER_CTX_free"
#endif
  evpCipherCtxFree :: Ptr EVP_CIPHER_CTX -> IO ()

foreign import ccall unsafe
#if OPENSSL_VERSION_NUMBER < 0x10100000L
  "EVP_MD_CTX_create"
#else
  "EVP_MD_CTX_new"
#endif
  evpMdCtxNew :: IO (Ptr EVP_MD_CTX)

foreign import ccall unsafe
#if OPENSSL_VERSION_NUMBER < 0x10100000L
  "EVP_MD_CTX_destroy"
#else
  "EVP_MD_CTX_free"
#endif
  evpMdCtxFree :: Ptr EVP_MD_CTX -> IO ()

foreign import ccall unsafe "EVP_PKEY_size" evpPKEYSize :: Ptr EVP_PKEY -> IO CInt

#if OPENSSL_VERSION_NUMBER < 0x10100000L
foreign import ccall unsafe "EVP_CIPHER_CTX_init" evpCipherCtxInit :: Ptr EVP_CIPHER_CTX -> IO ()
-- some implementations have this return int, but not all so we must ignore the return value.
foreign import ccall unsafe "EVP_CIPHER_CTX_cleanup" evpCipherCtxCleanup :: Ptr EVP_CIPHER_CTX -> IO ()

withCipherCtx :: (Ptr EVP_CIPHER_CTX -> IO a) -> IO a
withCipherCtx action = E.bracket_ evpCipherCtxInit evpCipherCtxCleanup action
#else
foreign import ccall "EVP_CIPHER_CTX_free_hsfun" evpCipherCtxFreePtr :: FunPtr (Ptr EVP_CIPHER_CTX -> IO ())
foreign import ccall "EVP_MD_CTX_free_hsfun" evpMdCtxFreePtr :: FunPtr (Ptr EVP_MD_CTX -> IO ())

withCipherCtx :: (Ptr EVP_CIPHER_CTX -> IO a) -> IO a
withCipherCtx action = do
    ptr <- evpCipherCtxNew
    fptr <- newForeignPtr evpCipherCtxFreePtr ptr
    withForeignPtr fptr action
#endif

withMdCtx :: (Ptr EVP_MD_CTX -> IO a) -> IO a
withMdCtx action = do
    ptr <- evpMdCtxNew
    fptr <- newForeignPtr evpMdCtxFreePtr ptr
    withForeignPtr fptr action

decryptAll :: BS.ByteString -> BS.ByteString -> EvpPkey -> BS.ByteString -> IO BS.ByteString
decryptAll keydata iv pkey xs = withCipherCtx $ \cctx -> do
    withData keydata $ \a b -> BS.useAsCStringLen iv $ \ (iv,_) -> do
    withForeignPtr pkey $ \pkey -> do
    throwZero_ "EVP_OpenInit" $ evpOpenInit cctx cipher a b (castPtr iv) pkey
    d <- evp_OpenUpdate cctx xs
    dr <- evp_OpenFinal cctx
    return $ d `BS.append` dr

cipher = unsafePerformIO evpDesEde3Cbc
md5 = unsafePerformIO evpMD5

verifyAll :: EvpPkey -> BS.ByteString -> BS.ByteString -> IO Bool
verifyAll pkey data_ sig = do
    withMdCtx $ \mdctx -> do
    evpDigestInit mdctx md5
    withData data_ $ \a b -> evpDigestUpdate mdctx a b
    withForeignPtr pkey $ \pk -> do
    withData sig $ \a b -> do
    rv <- evpVerifyFinal mdctx a b pk
    return $ case rv of
      1 -> True
      _ -> False

signAll :: EvpPkey -> BS.ByteString -> IO BS.ByteString
signAll  pkey xs = withMdCtx $ \cctx -> do
    evpSignInit cctx md5
    withData xs $ \a b -> (evpSignUpdate cctx a b)
    withForeignPtr pkey $ \pkey -> do
    bsz <- fmap fromIntegral $ evpPKEYSize pkey
    d <- returnData bsz (\outa outl -> throwZero_ "EVP_SignFinal" (evpSignFinal cctx outa outl pkey))
    return d


withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs ps action = fp' ps []   where
    fp' [] xs = action (reverse xs)
    fp' (p:ps) xs = withForeignPtr p (\x -> fp' ps (x:xs))

encryptAll :: [EvpPkey] -> BS.ByteString -> IO (BS.ByteString, [BS.ByteString], BS.ByteString)
encryptAll [] _ = error "encryptAll: no keys"
encryptAll  keys xs = doit' where
    doit' = do
            putLog LogDebug $ "encryptAll: " ++ show keys
            doit
    doit = do
        withCipherCtx $ \cctx -> do
        allocaArray n $ \ek -> do
        allocaArray n $ \ekl -> do
        allocaBytes ivs $ \iv -> do
        withForeignPtrs keys $ \keys -> do
        withArray keys $ \pubk -> do
            putLog LogDebug $ "encryptAll: " ++ show keys
            mapM_ (\n -> peekElemOff ek n >>= \v -> putLog LogDebug $ "ea: " ++ show (n,v)) [0 .. n - 1]
            bsz <- fmap (fi . maximum) $ mapM  ( evpPKEYSize) (keys)
            putLog LogDebug $ "encryptAll: bsz " ++ show bsz
            foldr (\n r -> allocaBytes bsz (\x -> pokeElemOff ek n x >> r)) (rest cctx ek ekl iv pubk) [0 .. n - 1]
    rest cctx ek ekl iv pubk = do
        mapM_ (\n -> peekElemOff ek n >>= \v -> putLog LogDebug $ "ea: " ++ show (n,v)) [0 .. n - 1]
        ---withForeignPtr pubk $ \pubk -> do
        throwZero_ "EVPSealInit" $ evpSealInit cctx cipher ek ekl iv pubk (fi n)
        bsize <- evpCipherContextBlockSize cctx
        putLog LogDebug $ "encryptAll: bsize " ++ show bsize
        rd <- returnData (dsize + bsize) $ \ra rb ->  withData xs $ \a b -> (throwZero_ "EVP_SealUpdate" $ evpSealUpdate cctx ra rb a b)
        d <- returnData bsize (\outa outl -> throwZero_ "EVP_SealFinal" (evpSealFinal cctx outa outl))
        iva <- BS.packCStringLen (castPtr iv, fi ivs) -- peekArray ivs (castPtr iv)
        ks <- mapM (pa ek ekl) [0 .. n - 1]
        return (rd `BS.append` d,ks,iva)
    pa ek ekl n = do
        l <- peekElemOff ekl n
        p <- peekElemOff ek n
        BS.packCStringLen (castPtr p,fi l) -- (fi l) (castPtr p)
    n = length keys
    ivs = 8
    dsize = BS.length xs
--    bsize = 64



--------------------
-- BigNum routines
--------------------

--bn_bin2bn :: [Word8] -> IO (Ptr BIGNUM)
--bn_bin2bn xs = throwIfNull "BN_bin2bn" $ withData xs (\a b -> bnBin2Bn a b nullPtr)

foreign import ccall unsafe "BN_bin2bn" bnBin2Bn :: Ptr CUChar -> CInt -> Ptr BIGNUM -> IO (Ptr BIGNUM)

newtype SHA_CTX = SHA_CTX (Ptr SHA_CTX)

foreign import ccall unsafe "SHA1_Init" sha1Init :: SHA_CTX -> IO ()
foreign import ccall unsafe "SHA1_Update" sha1Update :: SHA_CTX -> Ptr a -> CULong -> IO ()
foreign import ccall unsafe "SHA1_Final" sha1Final :: Ptr CChar -> SHA_CTX -> IO ()


sha1 :: LBS.ByteString -> BS.ByteString
sha1 lbs = unsafePerformIO $ withSHA_CTX $ \sctx -> do
    let supdate bs = BS.unsafeUseAsCStringLen bs $ \ (p,l) -> sha1Update sctx p (fromIntegral l)
    mapM_ supdate (LBS.toChunks lbs)
    allocaBytes (#const SHA_DIGEST_LENGTH) $ \pp -> do
        sha1Final pp sctx
        BS.packCStringLen (pp,(#const SHA_DIGEST_LENGTH))

bsToHex :: BS.ByteString -> String
bsToHex bs = BS.foldr f [] bs where
    f w = showHex x . showHex y where
        (x,y) = divMod w 16


withSHA_CTX :: (SHA_CTX -> IO a) -> IO a
withSHA_CTX action = allocaBytes (#const sizeof(SHA_CTX)) $ \cctx ->
    sha1Init (SHA_CTX cctx) >> action (SHA_CTX cctx)

foreign import ccall unsafe pkeyNewRSA :: RSA -> IO (Ptr EVP_PKEY)

-- foreign import ccall "&EVP_PKEY_free" evpPkeyFreePtr :: FunPtr (Ptr EVP_PKEY -> IO ())
foreign import ccall "get_KEY" evpPkeyFreePtr :: FunPtr (Ptr EVP_PKEY -> IO ())

#if OPENSSL_VERSION_NUMBER >= 0x10100000L
foreign import ccall unsafe "RSA_get0_n" rsaGet0N :: RSA -> IO (Ptr BIGNUM)
foreign import ccall unsafe "RSA_get0_e" rsaGet0E :: RSA -> IO (Ptr BIGNUM)
foreign import ccall unsafe "RSA_get0_d" rsaGet0D :: RSA -> IO (Ptr BIGNUM)
foreign import ccall unsafe "RSA_get0_dmp1" rsaGet0DMP1 :: RSA -> IO (Ptr BIGNUM)
foreign import ccall unsafe "RSA_get0_dmq1" rsaGet0DMQ1 :: RSA -> IO (Ptr BIGNUM)
foreign import ccall unsafe "RSA_get0_iqmp" rsaGet0IQMP :: RSA -> IO (Ptr BIGNUM)
foreign import ccall unsafe "RSA_set0_key" rsaSet0Key :: RSA -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> IO ()
foreign import ccall unsafe "RSA_set0_factors" rsaSet0Factors :: RSA -> Ptr BIGNUM -> Ptr BIGNUM -> IO ()
foreign import ccall unsafe "RSA_set0_crt_params" rsaSet0CrtParams :: RSA -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> IO ()
#endif

createPkey :: RSAElems BS.ByteString -> IO EvpPkey
createPkey re =  create_rsa re >>= create_pkey where
#if OPENSSL_VERSION_NUMBER < 0x10100000L
    setBn pb d = do
        np <- peek pb
        n <- withData d (\a b -> bnBin2Bn a b np)
        poke pb n

    rsaGet0N rsa = (#peek RSA, n) rsa
    rsaGet0E rsa = (#peek RSA, e) rsa
    rsaGet0D rsa = (#peek RSA, d) rsa
    rsaGet0DMP1 rsa = (#peek RSA, dmp1) rsa
    rsaGet0DMQ1 rsa = (#peek RSA, dmq1) rsa
    rsaGet0IQMP rsa = (#peek RSA, iqmp) rsa

    rsaSet0Factors rsa p q = do
        setBn ((#ptr RSA, p) rsa) (rsaP re)
        setBn ((#ptr RSA, q) rsa) (rsaQ re)

    rsaSet0CrtParams rsa dmp1 dmq1 iqmp = do
        setBn ((#ptr RSA, dmp1) rsa) (rsaDMP1 re)
        setBn ((#ptr RSA, dmq1) rsa) (rsaDMQ1 re)
        setBn ((#ptr RSA, iqmp) rsa) (rsaIQMP re)
#endif

    setNED rsa n e d = do
#if OPENSSL_VERSION_NUMBER < 0x10100000L
        when (n /= nullPtr) $ (#poke RSA, n) rsa n
        when (e /= nullPtr) $ (#poke RSA, e) rsa e
        when (d /= nullPtr) $ (#poke RSA, d) rsa d
#else
        rsaSet0Key rsa n e d
#endif

    create_private _ RSAElemsPublic {} = return ()
    create_private rsa re = do
        let d = rsaD re
            iqmp = rsaIQMP re
            p = rsaP re
            q = rsaQ re
            dmp1 = rsaDMP1 re
            dmq1 = rsaDMQ1 re
        _ <- copyBN d $ \dp -> setNED rsa nullPtr nullPtr dp
        _ <- copyBN dmp1 $ \dmp1p ->
               copyBN dmq1 $ \dmq1p ->
                 copyBN iqmp $ \iqmpp ->
                   rsaSet0CrtParams rsa dmp1p dmq1p iqmpp
        _ <- copyBN p $ \pp -> copyBN q $ \qp ->
               rsaSet0Factors rsa pp qp
        rsaCheckKey rsa
      where
        copyBN bs f =
          withData bs (\a b -> do
              p <- bnBin2Bn a b nullPtr
              f p)
    create_rsa re = do
        let n = rsaN re
            e = rsaE re
        rsa <- rsaNew
        np <- rsaGet0N rsa
        n <- withData n (\a b -> bnBin2Bn a b $ castPtr np)
        ep <- rsaGet0E rsa
        e <- withData e (\a b -> bnBin2Bn a b $ castPtr ep)
        setNED rsa n e nullPtr
        create_private rsa re
        return rsa
    create_pkey rsa = do
        pkey <- pkeyNewRSA rsa
        newForeignPtr evpPkeyFreePtr pkey


