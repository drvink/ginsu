module EIO(readRawFile,writeRawFile,  putRaw, readRaw,atomicWriteFile, getUniqueName, atomicWrite, getTempFileName, memoIO, withTempfile, hPutRawContents) where

import Data.Char
import Control.Monad
import Control.Exception as E
import Data.Array.IO
import Data.Unique
import System.Directory(removeFile)
import System.IO.Unsafe
import Data.IORef
import System.Posix
import Data.Word
import System.IO

bufSize = 4096

readRawFile :: String -> IO [Word8]
readRawFile fn = E.bracket (openBinaryFile fn ReadMode) hClose hGetRawContents

writeRawFile :: String -> [Word8] -> IO ()
writeRawFile fn xs = E.bracket (openBinaryFile fn WriteMode) hClose $ \h -> hPutRawContents h xs

hGetRawContents :: Handle -> IO [Word8]
hGetRawContents h = do
    a <- newArray_ (1,bufSize)
    getall a where
        getall a = do
            sz <- hGetArray h a bufSize
            av <- getElems a
            if sz == 0 then return [] else do
                r <- getall a
                return (take sz av ++ r)

hPutRawContents :: Handle -> [Word8] -> IO ()
hPutRawContents h xs = do
    a <- newArray_ (1,bufSize)
    prc a h xs where
        prc _ _ [] = return ()
        prc a h xs@(_:_) = do
            let (ys,zs) = splitAt bufSize xs
            if null zs
              then do -- work around a ghc bug in hPutArray
                let lys = length ys
                a' <- newListArray (1,lys) ys
                hPutArray h a' lys
              else do
                zipWithM_ (writeArray a) [1..] ys
                hPutArray h a bufSize
                prc a h zs


putRaw :: Handle -> [Word8] -> IO ()
putRaw h v = hPutStr h (map (chr . fromIntegral) v)

readRaw :: Handle -> Int -> IO [Word8]
readRaw _ 0 = return []
readRaw h n = do
    a <- newArray_ (0,(n - 1))
    sz <- hGetArray h a n
    av <- (getElems a)
    return $! (take sz av)
-- v <- replicateM n (hGetChar h)
--    return $ map (fromIntegral . ord) v


atomicWrite :: String -> (Handle -> IO a) -> IO a
atomicWrite fn action = do
    n <- getUniqueName
    let tn = fn ++ "." ++ n
    v <- E.bracket (openBinaryFile tn WriteMode) hClose action
    rename tn fn
    return v


atomicWriteFile :: String -> String -> IO ()
atomicWriteFile name s = do
    n <- getUniqueName
    let tn = name ++ "." ++ n
    writeFile tn s
    rename tn name


getUniqueName :: IO String
getUniqueName = do
    id <- getProcessID
    u <- newUnique
    n <- liftM nodeName getSystemID
    t <- epochTime
    return $ n ++ "." ++ show id ++ "." ++ show t ++ "." ++ show (hashUnique u)


memoIO :: IO a -> IO a
memoIO ioa = do
    v <- readIORef var
    case v of
        Just x -> return x
        Nothing -> do
            x <- ioa
            writeIORef var (Just x)
            return x
     where
        {-# NOTINLINE var #-}
        var = unsafePerformIO $ newIORef Nothing

getTempFileName :: IO String
getTempFileName = do
    u <- getUniqueName
    return $ "/tmp/" ++ u


withTempfile :: (String -> IO a) -> IO a
withTempfile action = E.bracket getTempFileName removeFile action

