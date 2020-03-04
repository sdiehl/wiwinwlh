{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -mavx #-}
{-# OPTIONS_GHC -msse #-}
{-# OPTIONS_GHC -msse2 #-}
{-# OPTIONS_GHC -msse4 #-}

import GHC.Exts
import GHC.Prim

data FloatX4 = FX4# FloatX4#

instance Show FloatX4 where
  show (FX4# f) = case unpackFloatX4# f of
    (# a, b, c, d #) -> show (F# a, F# b, F# c, F# d)

main :: IO ()
main = do
  let a = packFloatX4# (# 4.5#, 7.8#, 2.3#, 6.5# #)
  let b = packFloatX4# (# 8.2#, 6.3#, 4.7#, 9.2# #)
  let c = FX4# (broadcastFloatX4# 1.5#)
  print (FX4# a)
  print (FX4# (plusFloatX4# a b))
  print c
