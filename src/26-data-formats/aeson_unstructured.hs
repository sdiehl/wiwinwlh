{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Data.Aeson
import Data.Vector
import qualified Data.HashMap.Strict as M
import qualified Data.ByteString.Lazy as BL

-- Pull a key out of an JSON object.
(^?) :: Value -> Text -> Maybe Value
(^?) (Object obj) k = M.lookup k obj
(^?) _ _ = Nothing

-- Pull the ith value out of a JSON list.
ix :: Value -> Int -> Maybe Value
ix (Array arr) i = arr !? i
ix _ _ = Nothing

readJSON str = do
  obj <- decode str
  price <- obj ^? "price"
  refs  <- obj ^? "refs"
  tags  <- obj ^? "tags"
  aref  <- refs ^? "a"
  tag1  <- tags `ix` 0
  return (price, aref, tag1)

main :: IO ()
main = do
  contents <- BL.readFile "example.json"
  print $ readJSON contents
