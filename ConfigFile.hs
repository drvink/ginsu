module ConfigFile(
    configLookupBool,
    configLookup,
    configLookupList,
    configLookupElse,
    -- new interface
    configGet,
    configSetup,
    reloadConfigFiles,
    mapConfig,
    configFile,
    configEnv,
    configDefault,
    configShow,
    defaultConfig,
    parseConfigFile
    
    
    ) where
-- arch-tag: b78d53d4-380c-43d5-b885-5c433d38a8e0

import Char
import System
import System.IO.Unsafe

import CacheIO
import ErrorLog
import Data.Monoid

type Config = String -> IO [(String,(String,String))]
type ConfigFile = [(String, String)]

-- dealing with global config settings


{-# NOTINLINE config_default #-}
config_default :: JVar Config
config_default = unsafePerformIO (newJVar mempty)


configSetup :: Config -> IO ()
configSetup c = writeVal config_default c >> reloadConfigFiles

defaultConfig :: IO Config
defaultConfig = readVal config_default

configGet :: String -> IO [(String,(String,String))]
configGet k = do
        c <- defaultConfig
        v <- c k
        mapM fixUp v where
            fixUp (w,(k,v)) = do
                v' <- conv v 
                return (w,(k,v'))
            conv v = case (span (/= '$') v) of
                (xs,"") -> return xs
                (xs,('$':'$':ys)) -> conv ys >>= \r -> return (xs ++ "$" ++ r)
                (xs,('$':c:ys)) | isDigit c -> conv ys >>= \r -> return (xs ++ ['$',c] ++ r)
                (xs,('$':ys)) -> case span isPropName ys of
                    (pn,ys) -> do
                        n <- configLookupElse pn ""
                        r <- conv ys
                        return (xs ++ n ++ r)
                _ -> error "shouldn't happen"


-- | reload all configuration files emit a config signal.
reloadConfigFiles :: IO ()
reloadConfigFiles = writeVal config_files_var [] {->> signal configSignal ()-}




{-# NOTINLINE config_files_var #-}
config_files_var :: JVar [(String,ConfigFile)]
config_files_var = unsafePerformIO (newJVar [])


basicLookup n cl k = return [ (n,(k,v)) | (k',v) <- cl, k == k']

configDefault :: [(String,String)] -> Config 
configDefault cl k = basicLookup "default" cl k

configFile :: String -> Config
configFile fn k = do
    cf <- readVal config_files_var
    case lookup fn cf of 
	Just cl -> basicLookup fn cl k
	Nothing -> do 
	    cl <- catchMost (fmap parseConfigFile $ readFile fn) (\_ -> return []) 
	    mapVal config_files_var ((fn,cl):)
	    basicLookup fn cl k

configEnv :: Config
configEnv k = do
    ev <- catch (fmap return $ getEnv k) (\_ -> return [])
    return $ fmap (\v -> ("enviornment", (k,v))) ev

mapConfig :: (String -> String) -> Config -> Config
mapConfig f c s = c (f s) 
    
instance Monoid Config where
    mempty _ =  return []
    mappend c1 c2 s = do
	x <- c1 s 
	y <- c2 s
	return (x ++ y)

configShow :: [String] -> Config -> IO String
configShow ss c = do
    v <- mapM c ss
    return $ unlines $ map p $ zip ss v where
	p (k,((w,(k',v))):_) = k ++ " " ++ v ++ "\n#  in " ++ w ++ 
	    if k' /= k then " as " ++ k' else ""
	p (k,[]) = "#" ++ k ++ " Not Found."
    

-- types of config sources:
-- enviornment
-- enviornment after transformation of query
-- file 
-- default


    


isPropName c = isAlphaNum c || c `elem` "-_"

parseConfigFile :: String -> ConfigFile
parseConfigFile s = concatMap bl (fixup $ lines (uncomment s)) where
    uncomment ('#':xs) = uncomment (dropWhile (/= '\n') xs)
    uncomment ('-':'-':xs) = uncomment (dropWhile (/= '\n') xs)
    uncomment (x:xs) = x:uncomment xs
    uncomment [] = []
    fixup (x:y@(c:_):xs) | isSpace c = fixup ((x ++ y):xs)
    fixup (x:xs) = x: fixup xs
    fixup [] = []
    bl s = let (n,r) = span isPropName (dropWhile isSpace s) in
	if null n then [] else [(n,dropWhile isSpace r)]

{-

{-# NOINLINE config_file_var #-}
config_file_var :: SVar ConfigFile
config_file_var = unsafePerformIO $ newSVar []


loadStdConfiguration :: String -> IO ()
loadStdConfiguration f = do
    c <- getConfiguration f
    writeSVar config_file_var c
    return ()

getConfiguration :: String -> IO ConfigFile
getConfiguration s = do
    e <- getEnv "HOME"
    c <- readFile (e ++ "/" ++ s)
    return $ parseConfigFile c
-}

configLookupBool k = do
    x <- configLookup k
    case x of 
	Just s | cond -> return True where
	    cond = (map toLower s) `elem` ["true", "yes", "on", "y", "t"]
	_ -> return False
    

configLookup k = do
    vs <- configGet k
    case vs of 
        ((_,(_,v)):_) -> return $ Just v
        [] -> return Nothing


configLookupList k = do
    vs <- configGet k
    return $ [ y| (_,(_,y)) <- vs]

configLookupElse k e = do
    v <- configLookup k
    case v of
        Just v -> return v 
        Nothing -> return e

