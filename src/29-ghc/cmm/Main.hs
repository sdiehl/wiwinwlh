{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import GHC.Prim
import GHC.Word

foreign import prim "example" example_cmm :: Word# -> Word#

example :: Word64 -> Word64
example (W64# n) = W64# (example_cmm n)

main :: IO ()
main = print (example 2)
