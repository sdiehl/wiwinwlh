{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Lens
import Control.Lens.TH

data Record1 a = Record1
  { _a :: Int
  , _b :: Maybe a
  } deriving Show

data Record2 = Record2
  { _c :: String
  , _d :: [Int]
  } deriving Show

$(makeLenses ''Record1)
$(makeLenses ''Record2)

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

-- Some abstract traversals.
ids   = traverse.a
names = traverse.b._Just.c
nums  = traverse.b._Just.d
list2 = traverse.b._Just.d.ix 2

-- Modify/read/extract in terms of generic traversals.

-- Modify to set all 'id' fields to 0
ex1 = set ids 0 records

-- Return a view of the concatenated 'd' fields for all nested records.
ex2 = view nums records
-- [1,2,3,4,5,6,7,8,9]

-- Increment all 'id' fields by 1
ex3 = over ids (+1) records

-- Return a list of all 'c' fields.
ex4 = toListOf names records
-- ["Picard","Riker","Data"]

-- Return the the second element of all 'd' fields.
ex5 = toListOf list2 records
-- [3,6,9]
