module Gale.Proto where

import Atom
import Bits
import Char
import GenUtil
import PackedString
import Gale.Puff
import SimpleParser
import System
import System.Time
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get
import Word


pubkey_magic3 :: [Word8]
pubkey_magic3 = map (fromIntegral . ord) "GALE\000\001"

decodeFragments :: [Word8] -> FragmentList
decodeFragments [] = []
decodeFragments xs = decodeFragment ft f : decodeFragments r where
    (ft,xs') = xdrReadUInt xs
    (fl,xs'') = xdrReadUInt xs'
    (f,r) = splitAt (fromIntegral fl) xs''

decodeFragment :: Word32 -> [Word8] -> (Atom, Fragment)
decodeFragment t xs = (fn,f t) where
    (fnl,xs') = xdrReadUInt xs
    (fn,xs'') = liftT2 (fromString . galeDecodeString,id) (splitAt (fromIntegral $ fnl * 2) xs')
    f 0 = FragmentText $ packString (galeDecodeString xs'')
    f 1 = FragmentData $ BS.pack  xs''
    f 2 = FragmentTime $ decodeTime xs''
    f 3 = FragmentInt (fromIntegral $ fst $ xdrReadUInt xs'')
    f 4 = FragmentNest (decodeFragments xs'')
    f x = error $ "unknown fragment: " ++ show x

decodeTime :: [Word8] -> ClockTime
decodeTime xs = TOD (fromIntegral t) 0 where
    (t,_) = xdrReadUInt (drop 4 xs)


parseNullString :: GenParser Word8 String
parseNullString = (sat (== 0) >> return "") <|> (parseSome 1 >>= \[x] -> fmap ((chr $ fromIntegral x):) parseNullString)

galeDecodeString :: [Word8] -> String
galeDecodeString [] = []
galeDecodeString (b1:b2:xs) = (chr $ (fromIntegral b2) .|. (fromIntegral b1 `shiftL` 8)) : galeDecodeString xs
galeDecodeString _ = error "invalid gale string"




xdrReadUInt :: [Word8] -> ( Word32,[Word8])
xdrReadUInt (b1:b2:b3:b4:bs) = (x,bs) where
    x = (fromIntegral b4) .|. (fromIntegral b3 `shiftL` 8) .|.
	    (fromIntegral b2 `shiftL` 16) .|. (fromIntegral b1 `shiftL` 24)
xdrReadUInt bs = error $ "xdrReadUInt " ++ show bs

parseWord32 :: GenParser Word8 Word32
parseWord32 = do
    [b1,b2,b3,b4] <- parseSome 4
    return $ (fromIntegral b4) .|. (fromIntegral b3 `shiftL` 8) .|.
	    (fromIntegral b2 `shiftL` 16) .|. (fromIntegral b1 `shiftL` 24)

parseLenData = parseWord32 >>= parseSome

parseIntegral32 :: Integral a => GenParser Word8 a
parseIntegral32 = fmap fromIntegral parseWord32

parseLenString :: GenParser Word8 String
parseLenString = do
    len <- parseIntegral32
    w <- parseSome (len * 2)
    return $ galeDecodeString w
{-
decodeTime (x:y:_) | x > 0 || y > 0 = endOfTime
decodeTime xs = TimeDiff {tdDay = 0, tdPicosec = 0, tdYear = 0, tdMonth = 0, tdHour = 0, tdMin = fromIntegral (t `div` 60) , tdSec = fromIntegral (t `mod` 60)} `addToClockTime` epoch where
    (t,_) = xdrReadUInt (drop 4 xs)

-}

getGaleDir :: IO String
getGaleDir = do
    gd <- lookupEnv "GALE_DIR"
    case gd of
	Just v -> return $ v ++ "/"
	Nothing -> do
	    h <- getEnv "HOME"
	    return (h ++ "/.gale/")

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
    let x <+> y = x ++ " " ++ y
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
