{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PartialTypeSignatures #-}


data Tree a = Node a [Tree a]
  deriving (Show, Functor, Foldable, Traversable)

tree :: Maybe [Int]
tree = foldMap go (Node [1] [Node [2] [], Node [3,4] []])
  where
    go [] = Nothing
    go xs = Just xs
