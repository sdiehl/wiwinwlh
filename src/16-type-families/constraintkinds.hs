{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Data.HashSet
import Data.Hashable
import GHC.Exts (Constraint)

type family Con a :: Constraint

type instance Con [a] = (Ord a, Eq a)

type instance Con (HashSet a) = (Hashable a)

class Sized a where
  gsize :: Con a => a -> Int

instance Sized [a] where
  gsize = length

instance Sized (HashSet a) where
  gsize = size
