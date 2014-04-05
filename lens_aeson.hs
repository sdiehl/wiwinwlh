{-# LANGUAGE OverloadedStrings #-}

import Control.Lens

import Data.Aeson.Lens
import Data.Aeson (decode, Value)
import Data.ByteString.Lazy as BL

main = do
  contents <- BL.readFile "kiva.json"
  let Just json = decode contents :: Maybe Value

  let vals :: [Double]
      vals = json ^.. key "loans"
                    . values
                    . key "terms"
                    . key "local_payments"
                    . values
                    . key "amount"
                    . _Double
  print vals
