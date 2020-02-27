{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import Data.ByteString.Lazy.Char8 as BL
import Data.Text
import GHC.Generics

data Refs
  = Refs
      { a :: Text,
        b :: Text
      }
  deriving (Show, Generic, FromJSON, ToJSON)

data Data
  = Data
      { id :: Int,
        name :: Text,
        price :: Int,
        tags :: [Text],
        refs :: Refs
      }
  deriving (Show, Generic, FromJSON, ToJSON)

main :: IO ()
main = do
  contents <- BL.readFile "example.json"
  let Just dat = decode contents
  print $ name dat
  print $ a (refs dat)
  BL.putStrLn $ encode dat
