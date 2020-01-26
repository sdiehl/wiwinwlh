{-# LANGUAGE OverloadedStrings #-}

module AES where

import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import Crypto.Random.Types
import Data.ByteString

type AesKey = ByteString

genKey :: IO AesKey
genKey = getRandomBytes 32 -- AES256 key size

aesEncrypt :: ByteString -> AesKey -> Either CryptoError ByteString
aesEncrypt input sk =
  ctrCombine
    <$> init
    <*> pure nullIV
    <*> pure input
  where
    init :: Either CryptoError AES256
    init = eitherCryptoError $ cipherInit sk

aesDecrypt :: ByteString -> AesKey -> Either CryptoError ByteString
aesDecrypt = aesEncrypt

main :: IO ()
main = do
  key <- genKey
  let message = "The quick brown fox jumped over the lazy dog."
      mcipherText = aesEncrypt message key
  case mcipherText of
    Right cipherText -> do
      print cipherText
      print (aesDecrypt cipherText key)
    Left err -> print err
