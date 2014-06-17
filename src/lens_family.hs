{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Lens.Family
import Lens.Family.TH
import Lens.Family.Stock
import Data.Traversable

data Record1 = Record1
  { _a :: Int
  , _b :: Maybe Record2
  } deriving Show

data Record2 = Record2
  { _c :: String
  , _d :: [Int]
  } deriving Show

mkLenses ''Record1
mkLenses ''Record2

records :: [Record1]
records = [
    Record1 {
      _a = 1,
      _b = Nothing
    },
    Record1 {
      _a = 2,
      _b = Just $ Record2 {
        _c = "Picard",
        _d = [1,2,3]
      }
    },
    Record1 {
      _a = 3,
      _b = Just $ Record2 {
        _c = "Riker",
        _d = [4,5,6]
      }
    },
    Record1 {
      _a = 4,
      _b = Just $ Record2 {
        _c = "Data",
        _d = [7,8,9]
      }
    }
  ]

ids   = traverse.a
names = traverse.b._Just.c
nums  = traverse.b._Just.d

ex1 = set ids 0 records
ex2 = view nums records
ex3 = over ids (+1) records
ex4 = toListOf names records
