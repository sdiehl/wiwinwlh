{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Char

class Convertible a b where
  convert :: a -> b

instance Convertible Int Integer where
  convert = toInteger

instance Convertible Int Char where
  convert = chr

instance Convertible Char Int where
  convert = ord
