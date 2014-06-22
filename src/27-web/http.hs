{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Types
import Network.HTTP.Client
import Control.Applicative
import Control.Concurrent.Async

type URL = String

get :: Manager -> URL -> IO Int
get m url = do
  req <- parseUrl url
  statusCode <$> responseStatus <$> httpNoBody req m

single :: IO Int
single = do
  withManager defaultManagerSettings $ \m -> do
    get m "http://haskell.org"

parallel :: IO [Int]
parallel = do
  withManager defaultManagerSettings $ \m -> do
    -- Fetch w3.org 10 times concurrently
    let urls = replicate 10 "http://www.w3.org"
    mapConcurrently (get m) urls

main :: IO ()
main = do
  print =<< single
  print =<< parallel
