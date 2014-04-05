import Data.Csv

import Text.Show.Pretty

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL

type ErrorMsg = String
type CsvData = V.Vector (V.Vector BL.ByteString)

example :: FilePath -> IO (Either ErrorMsg CsvData)
example fname = do
  contents <- BL.readFile fname
  return $ decode NoHeader contents
