import Control.Monad.Trans
import DynFlags
import GHC
import GHC.LanguageExtensions.Type
import GHC.Paths
import GhcMonad
import HIE.Bios
import InteractiveEval
import Outputable

example :: Ghc ()
example = do
  cradle <- liftIO (loadImplicitCradle ".")
  comp <- liftIO $ getCompilerOptions "." cradle
  case comp of
    CradleSuccess r -> do
      liftIO (print "Success")
      session <- initSession r
      dflags <- getSessionDynFlags
      let dflags' = foldl xopt_set dflags [ImplicitPrelude]
      setSessionDynFlags
        dflags'
          { hscTarget = HscInterpreted,
            ghcLink = LinkInMemory,
            ghcMode = CompManager
          }
      liftIO (putStrLn (showSDoc dflags (ppr session)))
    CradleFail err -> liftIO $ print err
    CradleNone -> liftIO $ print "No cradle"
  pure ()

main :: IO ()
main = runGhc (Just GHC.Paths.libdir) example
