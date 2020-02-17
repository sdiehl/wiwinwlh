{-# LANGUAGE DeriveGeneric #-}

import Data.Text
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BL

import Control.Applicative

data Refs = Refs
  { a :: Text
  , b :: Text
  } deriving (Show,Generic)

data Data = Data
  { id    :: Int
  , name  :: Text
  , price :: Float
  , tags  :: [Text]
  , refs  :: Refs
  } deriving (Show,Generic)

instance FromJSON Data
instance FromJSON Refs
instance ToJSON Data
instance ToJSON Refs

main :: IO ()
main = do
  contents <- BL.readFile "example.json"
  let Just dat = decode contents
  print $ name dat
  print $ a (refs dat)
