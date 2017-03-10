{-# LANGUAGE OverloadedStrings #-}

import Data.Word
import Data.ByteString as S
import Data.ByteArray as B
import Data.Serialize

import Crypto.Error
import Crypto.Random
import Crypto.Random.Entropy (getEntropy)

import Crypto.PubKey.DH
import qualified Crypto.PubKey.Curve25519 as Curve25519

-- https://github.com/haskell-crypto/cryptonite/tree/3c087f0f4462df606524083699119445bb81dfa6/tests
-- https://github.com/centromere/cacophony/blob/80adb3c69dd850794b038a95364693d9503a24ce/src/Crypto/Noise/DH/Curve25519.hs
-- https://github.com/glguy/ssh-hans/blob/f49ef74a8a37ddff1f4748f46be949704d41557c/src/Network/SSH/Keys.hs

alicePrivate = throwCryptoError $ Curve25519.secretKey ("\x77\x07\x6d\x0a\x73\x18\xa5\x7d\x3c\x16\xc1\x72\x51\xb2\x66\x45\xdf\x4c\x2f\x87\xeb\xc0\x99\x2a\xb1\x77\xfb\xa5\x1d\xb9\x2c\x2a" :: ByteString)
alicePublic  = throwCryptoError $ Curve25519.publicKey ("\x85\x20\xf0\x09\x89\x30\xa7\x54\x74\x8b\x7d\xdc\xb4\x3e\xf7\x5a\x0d\xbf\x3a\x0d\x26\x38\x1a\xf4\xeb\xa4\xa9\x8e\xaa\x9b\x4e\x6a" :: ByteString)
bobPrivate   = throwCryptoError $ Curve25519.secretKey ("\x5d\xab\x08\x7e\x62\x4a\x8a\x4b\x79\xe1\x7f\x8b\x83\x80\x0e\xe6\x6f\x3b\xb1\x29\x26\x18\xb6\xfd\x1c\x2f\x8b\x27\xff\x88\xe0\xeb" :: ByteString)
bobPublic    = throwCryptoError $ Curve25519.publicKey ("\xde\x9e\xdb\x7d\x7b\x7d\xc1\xb4\xd3\x5b\x61\xc2\xec\xe4\x35\x37\x3f\x83\x43\xc8\x5b\x78\x67\x4d\xad\xfc\x7e\x14\x6f\x88\x2b\x4f" :: ByteString)

genKey :: IO (Curve25519.SecretKey, Curve25519.PublicKey)
genKey = do
  r <- getEntropy 32 :: IO ScrubbedBytes
  let sk = throwCryptoError . Curve25519.secretKey $ r
      pk = Curve25519.toPublic sk
  return (sk, pk)

dh :: Curve25519.SecretKey -> Curve25519.PublicKey -> ScrubbedBytes
dh sk pk = convert $ Curve25519.dh pk sk

main :: IO ()
main = do
  (sk, pk) <- genKey
  let res = B.convert (dh sk pk) :: ByteString
  print res

  (a, fn) <- runCurve25519dh
  print a
  let sharedKey = fn (B.convert pk)
  print sharedKey

-- | Implements key exchange as defined by
-- curve25519-sha256@libssh.org.txt
runCurve25519dh :: 
  IO (S.ByteString, S.ByteString -> Maybe S.ByteString)
  {- ^ local public, remote public -> shared key -}
runCurve25519dh =

     -- fails if key isn't 32 bytes long
  do CryptoPassed priv <-
       fmap Curve25519.secretKey (getRandomBytes 32 :: IO S.ByteString)

     -- Section 2: Transmit public key as "string"
     let raw_pub_s  = convert $ Curve25519.toPublic priv

         computeSecret raw_pub_c
             -- fails if key isn't 32 bytes long
           | CryptoPassed pub_c <- Curve25519.publicKey raw_pub_c

             -- Section 4.3: Treat shared key bytes as "integer"
           = Just $ B.convert $ Curve25519.dh pub_c priv

           | otherwise = Nothing

     return (raw_pub_s, computeSecret)
