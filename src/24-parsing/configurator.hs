{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import qualified Data.Configurator as C

data Config = Config
  { verbose      :: Bool
  , loggingLevel :: Int
  , logfile      :: FilePath
  , dbHost       :: Text
  , dbUser       :: Text
  , dbDatabase   :: Text
  , dbpassword   :: Maybe Text
  } deriving (Eq, Show)

readConfig :: FilePath -> IO Config
readConfig cfgFile = do
  cfg          <- C.load [C.Required cfgFile]
  verbose      <- C.require cfg "logging.verbose"
  loggingLevel <- C.require cfg "logging.loggingLevel"
  logFile      <- C.require cfg "logging.logfile"
  hostname     <- C.require cfg "database.hostname"
  username     <- C.require cfg "database.username"
  database     <- C.require cfg "database.database"
  password     <- C.lookup cfg "database.password"
  return $ Config verbose loggingLevel logFile hostname username database password

main :: IO ()
main = do
  cfg <-readConfig "example.config"
  print cfg
