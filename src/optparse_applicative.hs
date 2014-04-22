import Data.List
import Data.Monoid
import Options.Applicative

data Opts = Opts
  { _files :: [String]
  , _quiet :: Bool
  , _fast :: Speed
  }

data Speed = Slow | Fast

options :: Parser Opts
options = Opts <$> filename <*> quiet <*> fast
  where
    filename :: Parser [String]
    filename = many $ argument str $
         metavar "filename..."
      <> help "Input files"

    fast :: Parser Speed
    fast = flag Slow Fast $
         long "cheetah"
      <> help "Perform task quickly."

    quiet :: Parser Bool
    quiet = switch $
         long "quiet"
      <> help "Whether to shut up."

greet :: Opts -> IO ()
greet (Opts files quiet fast) = do
  putStrLn "reading these files:"
  mapM_ print files

  case fast of
    Fast -> putStrLn "quickly"
    Slow -> putStrLn "slowly"

  case quiet of
    True  -> putStrLn "quietly"
    False -> putStrLn "loudly"

opts :: ParserInfo Opts
opts = info (helper <*> options) fullDesc

main :: IO ()
main = execParser opts >>= greet
