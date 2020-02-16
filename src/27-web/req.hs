{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans
import Data.Aeson
import GHC.Generics
import Network.HTTP.Req

data Point = Point {x :: Int, y :: Int}
  deriving (Generic, ToJSON, FromJSON)

example :: IO ()
example = runReq defaultHttpConfig $ do
  -- GET request http response
  r <- req GET (https "w3.org") NoReqBody bsResponse mempty
  liftIO $ print (responseBody r)
  -- GET request json response
  r <- req GET (https "api.github.com" /: "users" /: "sdiehl") NoReqBody jsonResponse mempty
  liftIO $ print (responseBody r :: Value)
  -- POST request json payload
  r <- req POST (https "example.com") (ReqBodyJson (Point 1 2)) jsonResponse mempty
  liftIO $ print (responseBody r :: Value)
