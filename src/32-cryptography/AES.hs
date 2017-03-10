{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..),Cipher(..),nullIV)
import Crypto.Error (CryptoFailable(..))

-- https://github.com/Risto-Stevcev/haskell-crypto-simple/blob/master/src/Crypto/Simple/CBC.hs

newtype Key a = Key ByteString
    deriving (Show,Eq)

secretKey :: ByteString
secretKey = "012-456-89A-CDE-012-456-89A-CDE-"

encrypt :: ByteString -> ByteString -> ByteString
encrypt secret = ctrCombine ctx nullIV
  where
    ctx :: AES256
    ctx = cipherInitNoErr (cipherMakeKey (undefined :: AES256) secret)

    cipherInitNoErr :: BlockCipher c => Key c -> c
    cipherInitNoErr (Key k) = case cipherInit k of
      CryptoPassed a -> a
      CryptoFailed e -> error (show e)

    cipherMakeKey :: Cipher cipher => cipher -> ByteString -> Key cipher
    cipherMakeKey = const Key


decrypt :: ByteString -> ByteString -> ByteString
decrypt = encrypt

foo :: ByteString
foo = encrypt secretKey "foo"

bar :: ByteString
bar = decrypt secretKey foo

main :: IO ()
main = do
  let msg = "The quick brown fox jumped over the lazy dog"
  let res = encrypt secretKey msg
  let dec = decrypt secretKey res
  print res
  print dec
