{-# LANGUAGE DeriveFoldable #-}

data RoseTree a
  = RoseTree a [RoseTree a]
  deriving (Foldable)

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)
  deriving (Foldable)
