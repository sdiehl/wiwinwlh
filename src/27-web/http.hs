{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Concurrent.Async
import Network.HTTP.Client
import Network.HTTP.Types

type URL = String

get :: Manager -> URL -> IO Int
get m url = do
  req <- parseUrlThrow url
  statusCode . responseStatus <$> httpNoBody req m

single :: IO Int
single = do
  manager <- newManager defaultManagerSettings
  get manager "http://haskell.org"

parallel :: IO [Int]
parallel = do
  manager <- newManager defaultManagerSettings
  -- Fetch w3.org 10 times concurrently
  let urls = replicate 10 "http://www.w3.org"
  mapConcurrently (get manager) urls

main :: IO ()
main = do
  print =<< single
  print =<< parallel
