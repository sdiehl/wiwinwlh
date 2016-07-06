{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Text
import Data.Aeson
import Data.Maybe
import Data.Aeson.Types
import Control.Applicative
import qualified Data.ByteString.Lazy as BL

data Crew = Crew
  { name  :: Text
  , rank  :: Rank
  } deriving (Show)

data Rank
  = Captain
  | Ensign
  | Lieutenant
  deriving (Show)

-- Custom JSON Deserializer

instance FromJSON Crew where
  parseJSON (Object o) = do
    _name <- o .: "name"
    _rank <- o .: "rank"
    pure (Crew _name _rank)

instance FromJSON Rank where
  parseJSON (String s) = case s of
    "Captain"    -> pure Captain
    "Ensign"     -> pure Ensign
    "Lieutenant" -> pure Lieutenant
    _            -> typeMismatch "Could not parse Rank" (String s)
  parseJSON x = typeMismatch "Expected String" x

-- Custom JSON Serializer

instance ToJSON Crew where
  toJSON (Crew name rank) = object [
      "name" .= name
    , "rank" .= rank
    ]

instance ToJSON Rank where
  toJSON Captain    = String "Captain"
  toJSON Ensign     = String "Ensign"
  toJSON Lieutenant = String "Lieutenant"


roundTrips :: Crew -> Bool
roundTrips = isJust . go
  where
    go :: Crew -> Maybe Crew
    go = decode . encode


picard :: Crew
picard = Crew { name = "Jean-Luc Picard", rank = Captain }

main :: IO ()
main = do
  contents <- BL.readFile "crew.json"
  let (res :: Maybe Crew) = decode contents
  print res
  print $ roundTrips picard
