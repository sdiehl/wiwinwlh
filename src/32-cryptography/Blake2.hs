{-# LANGUAGE OverloadedStrings #-}

module Blake2 where

import Crypto.Hash
import Data.ByteString

passHash :: Digest Blake2b_256
passHash = hash ("hunter2" :: ByteString)
