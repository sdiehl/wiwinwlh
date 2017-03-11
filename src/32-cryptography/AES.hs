{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..),Cipher(..),nullIV)
import Crypto.Error (CryptoFailable(..))

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
  let ctext = encrypt secretKey msg
  let ptext = decrypt secretKey ctext
  putStrLn "Crypotext:"
  print ctext
  putStrLn "Decrypted plaintext:"
  print ptext
