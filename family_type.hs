{-# LANGUAGE TypeFamilies #-}

import Data.Char

type family Rep a :: *
type instance Rep Int  = Char
type instance Rep Char = Int

class Convertible a where
  convert :: a -> Rep a

instance Convertible Int where
  convert = chr

instance Convertible Char where
  convert = ord
