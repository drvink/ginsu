
-- arch-tag: 40ae7e96-571c-4e68-a110-a2890f802428

module Regex where

import Text.Regex.Posix
import Char
import System.IO.Unsafe
import Monad
import GenUtil
import PackedString
import Maybe
import ConfigFile
import Control.Exception

subst :: String -> [String] -> String
subst "" _ = ""
subst ('$':'$':cs) xs  = '$':subst cs xs 
subst ('$':c:cs) xs | isDigit c = f xs (ord c - ord '0') ++ subst cs xs where
    f (x:_) 0 = x
    f (_:xs) n = f xs (n - 1)
    f _ _ = ""
subst (c:cs) xs = c:subst cs xs


matches :: Regex -> String -> [[String]]
matches rx s = case unsafePerformIO (regexec rx s) of
    Nothing -> []
    Just (_,v,r,xs) -> (v:xs):matches rx r


matchWords :: [(Regex,String,String)] -> String -> [(String,String)]
matchWords ((rx,a,b):rs) s = (map f $ matches rx s) ++ matchWords rs s where
    f xs = (subst a xs, subst b xs)
matchWords [] _ = []


buildMatchTable :: IO [(Regex,String,String)]
buildMatchTable = do
    hs <- configLookupList "apphook"
    let zs = catMaybes $ snds $ snubFst $ concatMap (f . simpleUnquote) (hs)
        f [n] = [(n,Nothing)]
        f [n,re,e] | Just rx <- compileRx re = [(n,Just (rxRegex rx, e, "$0"))]
        f [n,re,e,p] | Just rx <- compileRx re = [(n,Just (rxRegex rx, e, p))]
        f _ = []
    return zs
    

data Rx = Rx { rxString :: String, rxRegex :: Regex }

compileRx :: Monad m => String -> m Rx
compileRx re = liftM (Rx re) $ unsafePerformIO ( handle (\e -> return (fail $ show e)) (regcomp re' flags >>= return . return )) where
    flags = regExtended + ci + ml
    ci = if 'i' `elem` fl then regIgnoreCase else 0
    ml = if 'm' `elem` fl then regNewline else 0
    (fl,re') = ef re
    ef ('(':'?':cs) = let (a,b) = span (/= ')') cs in (a,drop 1 b)
    ef xs = ("",xs)


instance Show Rx where
    show (Rx s _) = s

matchRx re body = isJust (unsafePerformIO $ regexec (rxRegex re) (unpackPS body)) 

