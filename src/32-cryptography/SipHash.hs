{-# LANGUAGE OverloadedStrings #-}

module SipHash where

import Data.Word
import Data.ByteString
import Data.ByteArray.Hash

k0, k1 :: Word64
k0 = 0x4a7330fae70f52e8
k1 = 0x919ea5953a9a1ec9

msg :: ByteString
msg = "The quick brown fox jumped over the lazy dog"

hashed :: SipHash
hashed = sipHash (SipKey k0 k1) msg
