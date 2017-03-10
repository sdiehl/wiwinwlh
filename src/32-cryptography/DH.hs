{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

import Crypto.Random
import Crypto.PubKey.DH as DH

-- https://github.com/vincenthz/hs-tls/blob/1415d7330d90049fc5f813afb7fcc116a31b6f7d/core/Network/TLS/Crypto/DH.hs

type DHPublic   = DH.PublicNumber
type DHPrivate  = DH.PrivateNumber
type DHParams   = DH.Params
type DHKey      = DH.SharedKey

-- | The second Oakley group from RFC 2409, which provides ~1024 bits of
-- security.
oakley2 :: DH.Params
oakley2 = Params {
     params_p = 0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381FFFFFFFFFFFFFFFF
   , params_g = 2
   , params_bits = 1024
   }

dhGenerateKeyPair :: MonadRandom r => DHParams -> r (DHPrivate, DHPublic)
dhGenerateKeyPair params = do
    priv <- DH.generatePrivate params
    let pub = DH.generatePublic params priv
    return (priv, pub)

test :: IO (DHPrivate, DHPublic)
test = dhGenerateKeyPair oakley2
