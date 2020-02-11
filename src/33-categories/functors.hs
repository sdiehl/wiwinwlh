{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Category
import Prelude hiding (Functor, fmap, id)

class (Category c, Category d) => Functor c d t where
  fmap :: c a b -> d (t a) (t b)

type Hask = (->)

instance Functor Hask Hask [] where
  fmap f [] = []
  fmap f (x : xs) = f x : (fmap f xs)
