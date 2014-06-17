{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}

import GHC.Exts
import GHC.Prim

ex1 :: Bool
ex1 = gtChar# a# b#
  where
    !(C# a#) = 'a'
    !(C# b#) = 'b'

ex2 :: Int
ex2 = I# (a# +# b#)
  where
    !(I# a#) = 1
    !(I# b#) = 2

ex3 :: Int
ex3 = (I# (1# +# 2# *# 3# +# 4#))

ex4 :: (Int, Int)
ex4 = (I# (dataToTag# False), I# (dataToTag# True))
