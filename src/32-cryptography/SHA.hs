{-# LANGUAGE OverloadedStrings #-}

import Crypto.Hash (SHA256, Digest, hash)
import Data.ByteArray (convert)
import Data.ByteString.Char8 (ByteString)

v1 :: ByteString
v1 = "The quick brown fox jumps over the lazy dog"

h1 :: Digest SHA256
h1 = hash v1

s1 :: ByteString
s1  = convert h1

main :: IO ()
main = do
  print v1
  print h1
  print s1
