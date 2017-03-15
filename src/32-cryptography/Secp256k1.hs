{-# LANGUAGE OverloadedStrings #-}

module Secp256k1 where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..),nullIV)
import Crypto.Error (CryptoFailable(..), CryptoError(..))

import qualified Crypto.PubKey.ECC.DH as DH
import qualified Crypto.PubKey.ECC.Types as ECC

import qualified Crypto.Random.Types as CRT

import Data.ByteArray
import Data.ByteString (ByteString)

-- | ScrubbedBytes because DH.SharedKey 
newtype Key a = Key ScrubbedBytes

secp256k1 :: ECC.Curve
secp256k1 = ECC.getCurveByName ECC.SEC_p256k1

-- | Generate key pair
generate :: CRT.MonadRandom m => m (DH.PrivateNumber, DH.PublicPoint)
generate = do
  priv <- DH.generatePrivate secp256k1
  let pub = DH.calculatePublic secp256k1 priv
  pure (priv, pub)

-- | Compute shared secret
sharedSecret :: DH.PrivateNumber -> DH.PublicPoint -> DH.SharedKey
sharedSecret = DH.getShared secp256k1

-- Encrypt using AES256 and the ECDH shared secret
encrypt :: ScrubbedBytes -> ByteString -> Either CryptoError ByteString
encrypt secret msg = 
    case ctx of
      Left e -> Left e
      Right c -> Right $ ctrCombine c nullIV msg
  where
    ctx = initCipher (mkCipherKey (undefined :: AES256) secret)
    
    initCipher :: BlockCipher c => Key c -> Either CryptoError c
    initCipher (Key k) = case cipherInit k of
      CryptoPassed a -> Right a
      CryptoFailed e -> Left e
                                          
    mkCipherKey :: Cipher cipher => cipher -> ScrubbedBytes -> Key cipher
    mkCipherKey _ = Key 

decrypt :: ScrubbedBytes -> ByteString -> Either CryptoError ByteString
decrypt = encrypt

example :: IO ()
example = do
    (alicePrivKey, alicePubKey) <- generate
    (bobPrivKey, bobPubKey) <- generate 
   
    -- | Calculate shared secrets
    let DH.SharedKey aliceSK = sharedSecret alicePrivKey bobPubKey  
        DH.SharedKey bobSK = sharedSecret bobPrivKey alicePubKey

    print (aliceSK == bobSK)
    let msg = "Haskell Crypto is fun."
   
    -- | Alice sends msg to Bob
    let eRes = encrypt aliceSK msg >>= decrypt bobSK  
    case eRes of
      Left e -> fail $ show e
      Right msg' -> do
        putStrLn $ "Alice's original msg: " ++ show msg
        putStrLn $ "Bob's decrypted msg from Alice: " ++ show msg'
