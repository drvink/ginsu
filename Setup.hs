import Control.Exception (bracketOnError)
import Control.Monad
import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import System.Directory
import System.Exit
import System.IO
import System.Process

genPP :: PPSuffixHandler
genPP = ("hsgen", \_ _ -> PreProcessor True $ mkSimplePreProcessor ppf) where
  ppf inf outf verb = do
    x <- executable `liftM` getPermissions inf
    cmd <- if x
      then (`proc` []) `liftM` canonicalizePath inf
      else shell `liftM` readFile inf
    bracketOnError (return outf) removeFile $ \f ->
      withFile f WriteMode $ \out -> do
      info verb $ scs (cmdspec cmd) ++ " > " ++ outf
      (_,_,_,pid) <- createProcess cmd { std_out = UseHandle out }
      r <- waitForProcess pid
      when (r /= ExitSuccess) $ Distribution.Simple.Utils.die $ scs (cmdspec cmd) ++ ": " ++ show r
  scs (ShellCommand s) = s
  scs (RawCommand c a) = unwords (c:a)

main = defaultMainWithHooks autoconfUserHooks { hookedPreProcessors = genPP : hookedPreProcessors autoconfUserHooks }
