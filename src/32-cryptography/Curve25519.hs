import Crypto.Error
import qualified Crypto.PubKey.Curve25519 as Curve25519

-- Diffie-Hellman Key Exchange for Curve25519
dh :: IO ()
dh = do
  alicePriv <- Curve25519.generateSecretKey
  bobPriv <- Curve25519.generateSecretKey
  let secret1 = Curve25519.dh (Curve25519.toPublic alicePriv) bobPriv
  let secret2 = Curve25519.dh (Curve25519.toPublic bobPriv) alicePriv
  print (secret1 == secret2)
