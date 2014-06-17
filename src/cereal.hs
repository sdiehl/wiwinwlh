{-# LANGUAGE DeriveGeneric #-}

import Data.Word
import Data.ByteString
import Data.Serialize

import GHC.Generics

data Val = A [Val] | B [(Val, Val)] | C
  deriving (Generic, Show)

instance Serialize Val where

encoded :: ByteString
encoded = encode (A [B [(C, C)]])
-- "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\STX\STX"

bytes :: [Word8]
bytes = unpack encoded
-- [0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,2,2]

decoded :: Either String Val
decoded = decode encoded
