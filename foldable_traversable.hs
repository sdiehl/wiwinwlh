import Data.Monoid
import Data.Foldable
import Data.Traversable

import Control.Applicative
import Control.Monad.Identity (runIdentity)
import Prelude hiding (mapM_, foldr)

-- Rose Tree
data Tree a = Node a [Tree a] deriving (Show)

instance Functor Tree where
  fmap f (Node x ts) = Node (f x) (fmap (fmap f) ts)

instance Traversable Tree where
  traverse f (Node x ts) = Node <$> f x <*> traverse (traverse f) ts

instance Foldable Tree where
  foldMap f (Node x ts) = f x `mappend` foldMap (foldMap f) ts


tree :: Tree Integer
tree = Node 1 [Node 1 [], Node 2 [] ,Node 3 []]


example1 :: IO ()
example1 = mapM_ print tree

example2 :: Integer
example2 = foldr (+) 0 tree

example3 :: Maybe (Tree Integer)
example3 = traverse (\x -> if x > 2 then Just x else Nothing) tree

example4 :: Tree Integer
example4 = runIdentity $ traverse (\x -> pure (x+1)) tree
