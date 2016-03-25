{-# LANGUAGE OverloadedStrings #-}

import Database.Redis
import Data.ByteString.Char8

session :: Redis (Either Reply (Maybe ByteString))
session = do
  set "hello" "haskell"
  get "hello"

main :: IO ()
main = do
  conn <- connect defaultConnectInfo
  res <- runRedis conn session
  print res
