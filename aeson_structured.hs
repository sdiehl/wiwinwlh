{-# LANGUAGE DeriveGeneric #-}

import Data.Text
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BL

import Control.Applicative

data Refs = Refs
  { a :: String
  , b :: String
  } deriving (Show,Generic)

data Data = Data
  { id    :: Int
  , name  :: Text
  , price :: Int
  , tags  :: [String]
  , refs  :: Refs
  } deriving (Show,Generic)

instance FromJSON Data
instance FromJSON Refs
instance ToJSON Data
instance ToJSON Refs

main :: IO ()
main = do
  contents <- BL.readFile "example.json"
  let dat = decode contents
  print $ name <$> dat
  print $ a <$> refs <$> dat
