{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString as BL
import Data.Text (Text)
import Data.Yaml
import GHC.Generics

data Invoice
  = Invoice
      { invoice :: Int,
        date :: Text,
        bill :: Billing
      }
  deriving (Show, Generic, FromJSON)

data Billing
  = Billing
      { address :: Address,
        family :: Text,
        given :: Text
      }
  deriving (Show, Generic, FromJSON)

data Address
  = Address
      { lines :: Text,
        city :: Text,
        state :: Text,
        postal :: Int
      }
  deriving (Show, Generic, FromJSON)

main :: IO ()
main = do
  contents <- BL.readFile "example.yaml"
  let (res :: Either ParseException Invoice) = decodeEither' contents
  case res of
    Left err -> print err
    Right val -> print val
