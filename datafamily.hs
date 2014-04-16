{-# LANGUAGE TypeFamilies #-}

import qualified Data.Vector.Unboxed as V

data family Array a
data instance Array Int       = IArray (V.Vector Int)
data instance Array Bool      = BArray (V.Vector Bool)
data instance Array (a,b)     = PArray (Array a) (Array b)
data instance Array (Maybe a) = MArray (V.Vector Bool) (Array a)

class IArray a where
  index :: Array a -> Int -> a

instance IArray Int where
  index (IArray xs) i = xs V.! i

instance IArray Bool where
  index (BArray xs) i = xs V.! i

-- Vector of pairs
instance (IArray a, IArray b) => IArray (a, b) where
  index (PArray xs ys) i = (index xs i, index ys i)

-- Vector of missing values
instance (IArray a) => IArray (Maybe a) where
  index (MArray bm xs) i =
    case bm V.! i of
      True  -> Nothing
      False -> Just $ index xs i
