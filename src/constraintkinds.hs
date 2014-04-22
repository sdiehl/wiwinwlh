{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

import GHC.Exts (Constraint)
import Data.Hashable
import Data.HashSet

type family Con a :: Constraint
type instance Con [a] = (Ord a, Eq a)
type instance Con (HashSet a) = (Hashable a)

class Sized a where
  gsize :: Con a => a -> Int

instance Sized [a] where
  gsize = length

instance Sized (HashSet a) where
  gsize = size
