{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -O1 #-}

module Main where

import GHC.Exts
import GHC.Base
import Foreign

data Size = Size
  { ptrs  :: Int
  , nptrs :: Int
  , size  :: Int
  } deriving (Show)

unsafeSizeof :: a -> Size
unsafeSizeof a =
  case unpackClosure# a of
    (# x, ptrs, nptrs #) ->
      let header  = sizeOf (undefined :: Int)
          ptr_c   = I# (sizeofArray# ptrs)
          nptr_c  = I# (sizeofByteArray# nptrs) `div` sizeOf (undefined :: Word)
          payload = I# (sizeofArray# ptrs +# sizeofByteArray# nptrs)
          size    = header + payload
      in Size ptr_c nptr_c size

data A = A {-# UNPACK #-} !Int
data B = B Int

main :: IO ()
main = do
  print (unsafeSizeof (A 42))
  print (unsafeSizeof (B 42))
