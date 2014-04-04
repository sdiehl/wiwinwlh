{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Csv
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL

data Plant = Plant
  { sepal_length :: Double
  , sepal_width  :: Double
  , petal_length :: Double
  , petal_width  :: Double
  , plant_class :: String
  } deriving (Generic, Show)

instance FromNamedRecord Plant
instance ToNamedRecord Plant

type ErrorMsg = String
type CsvData = (Header, V.Vector Plant)

parseCSV :: FilePath -> IO (Either ErrorMsg CsvData)
parseCSV fname = do
  contents <- BL.readFile fname
  return $ decodeByName contents

main = parseCSV "iris.csv" >>= print
