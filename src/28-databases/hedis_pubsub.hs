{-# LANGUAGE OverloadedStrings #-}

import Database.Redis

import Control.Monad
import Control.Monad.Trans
import Data.ByteString.Char8

import Control.Concurrent

subscriber :: Redis ()
subscriber =
 pubSub (subscribe ["news"]) $ \msg -> do
   print msg
   return mempty

publisher :: Redis ()
publisher = forM_ [1..100] $ \n -> publish "news" (pack (show n))

-- connects to localhost:6379
main :: IO ()
main = do
  conn1 <- connect defaultConnectInfo
  conn2 <- connect defaultConnectInfo

  -- Fork off a publisher
  forkIO $ runRedis conn1 publisher

  -- Subscribe for messages
  runRedis conn2 subscriber
