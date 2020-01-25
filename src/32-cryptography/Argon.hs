{-# LANGUAGE OverloadedStrings #-}

module Argon where

import Crypto.Error
import Crypto.KDF.Argon2
import Crypto.Random (getRandomBytes)
import Data.ByteString

passHash :: IO ()
passHash = do
  salt <- getRandomBytes 16 :: IO ByteString
  out <- throwCryptoErrorIO (hash defaultOptions ("hunter2" :: ByteString) salt 256)
  print (out :: ByteString)
