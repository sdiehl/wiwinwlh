import Control.Monad.IO.Class
import System.Console.Haskeline

process :: String -> IO ()
process = putStrLn

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Repl> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop
