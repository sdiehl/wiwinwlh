{-# LANGUAGE OverloadedStrings #-}

module Ed25519 where

import Crypto.PubKey.Ed25519 as Ed25519
import Data.ByteString

msg :: ByteString
msg = "My example message"

example :: IO ()
example = do
  privKey <- Ed25519.generateSecretKey
  let pubKey = Ed25519.toPublic privKey
  let sig = sign privKey pubKey msg
  print sig
  print (Ed25519.verify pubKey msg sig)
