{-# OPTIONS_GHC -fallow-overlapping-instances #-}

module Status(set,setS,clear, Status.get, setF, setFS, getStatus, Status.log, getLog) where


import System.IO.Unsafe
import Data.IORef
import Data.FiniteMap 
import GenUtil
import Data.Tree
import Char(chr)
import Doc.Chars
import List(intersperse,groupBy)
import CircularBuffer as CB


{-# NOINLINE status_var #-}
status_var :: IORef (FiniteMap String (IO String))
status_var  = unsafePerformIO $ newIORef emptyFM

{-# NOINLINE log_var #-}
log_var :: CB.CircularBuffer String
log_var = unsafePerformIO $ CB.new 10

log :: String -> IO ()
log s = CB.append log_var [s]

getLog :: IO [String]
getLog = CB.toList log_var



modify r f = atomicModifyIORef r (\x -> (f x,())) 

setS :: Show a => String -> a -> IO ()
setS w v = set w (show v) 

set :: String -> String -> IO ()
set w v = modify status_var (\fm -> addToFM fm w (return v))  

setF :: String -> IO String -> IO ()
setF w v = modify status_var (\fm -> addToFM fm w v)  

setFS :: Show a => String -> IO a -> IO ()
setFS w v = modify status_var (\fm -> addToFM fm w (fmap show v))  


get :: String -> IO (IO String)
get k = do
    fm <- readIORef status_var
    case lookupFM fm k of
        Just x -> return x
        Nothing -> return (return "")
    


clear :: String -> IO ()
clear k =  modify status_var (\fm -> delFromFM fm k)  


getall = do
    fm <- readIORef status_var
    return $ fmToList fm

getTree :: IO (Forest (String,String))
getTree = do
    xs <- getall
    let f (a,b) = do b <- b; return (split (== '.') a,b)
    xs <- mapM f xs 
    return $ createForest  xs 

createForest  xs = map f gs where
    --[Node (concat $ intersperse "." (xs),y) [] | (xs,y) <- xs] 
    f [(xs,ys)] =  Node (concat $ intersperse "." (xs),ys) []
    f xs@((x:_,_):_) = Node (x,"") (createForest [ (xs,ys) | (_:xs,ys)<- xs])
    f _ = error "createForest: should not happen."
    gs = groupBy (\(x:_,_) (y:_,_) -> x == y) xs
--createForest  xs = Node ("","") [ createTree [(xs,y)] | (xs,y) <- xs]

draw :: Tree String -> [String]
draw (Node x ts0) = x : drawSubTrees ts0
  where drawSubTrees [] = []
        drawSubTrees [t] =
                {-[vLine] :-} shift [chr 0x2570, chr 0x2574] "  " (draw t)
        drawSubTrees (t:ts) =
                {-[vLine] :-} shift (lTee ++ [chr 0x2574]) (vLine ++  " ") (draw t) ++ drawSubTrees ts

        shift first other = zipWith (++) (first : repeat other)
        --vLine = chr 0x254F

getStatus :: IO String
getStatus = do
    t <- getTree 
    let f (xs,"") = xs
        f (xs,ys) = xs ++ ": "  ++ ys
    return $ unlines (concatMap (draw . fmap f) t)
    
    


