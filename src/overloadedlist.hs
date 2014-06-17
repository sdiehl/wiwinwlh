{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

import qualified Data.Map as Map
import GHC.Exts (IsList(..))

instance (Ord k) => IsList (Map.Map k v) where
  type Item (Map.Map k v) = (k,v)
  fromList = Map.fromList
  toList = Map.toList

example1 :: Map.Map String Int
example1 = [("a", 1), ("b", 2)]
