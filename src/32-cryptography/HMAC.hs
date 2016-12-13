{-# LANGUAGE OverloadedStrings #-}


import Crypto.Hash (SHA256, Digest, hash)
import Crypto.MAC.HMAC (HMAC(..), hmac)
import Data.ByteArray (convert)
import Data.ByteString.Char8 (ByteString)

msg :: ByteString
msg = "The quick brown fox jumps over the lazy dog"

key :: ByteString
key = "hunter2"

digest :: HMAC SHA256
digest = hmac msg key

d1 :: Digest SHA256
d1 = hmacGetDigest digest

s1 :: ByteString
s1 = convert (hmacGetDigest digest)

main :: IO ()
main = do
  print d1
  print s1
