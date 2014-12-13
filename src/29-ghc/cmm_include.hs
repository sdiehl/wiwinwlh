-- ghc -c factorial.cmm -o factorial.o
-- ghc factorial.o Example.hs -o Example

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import GHC.Prim
import GHC.Word

foreign import prim "factorial" factorial_cmm  :: Word# -> Word#

factorial :: Word64 -> Word64
factorial (W64# n) =  W64# (factorial_cmm n)

main :: IO ()
main = print (factorial 5)
