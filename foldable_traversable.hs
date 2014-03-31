-- how to implement foldable and traversable

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.Monoid
import Data.Foldable
import Data.Traversable

import Control.Applicative
import Prelude hiding (mapM_, foldr)

data Tree a
  = Node a [Tree a]
  deriving (Show)

instance Functor Tree where
  fmap f (Node x ts) = Node (f x) (fmap (fmap f) ts)

instance Traversable Tree where
  traverse f (Node x ts) = Node <$> f x <*> traverse (traverse f) ts

instance Foldable Tree where
  foldMap f (Node x ts) = f x `mappend` foldMap (foldMap f) ts

tree = Node 1 [Node 1 [], Node 2 [] ,Node 3 []]

example1 = mapM_ print tree
example2 = foldr (+) 0 tree
example3 = traverse (\x -> if x > 2 then Just x else Nothing) tree

{-

deriving instance Functor Tree
deriving instance Traversable Tree
deriving instance Foldable Tree

-}

{-
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f a [b1, b2, b3, b4]
=> f (f (f (f a b1) b2) b3) b4
-}

{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f a [b1, b2, b3, b4]
=> f b1 (f b2 (f b3 (f b4 a)))
-}
