{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

data Size = None | Many

data List a b where
  Nil  :: List None a
  Cons :: a -> List b a -> List Many a

head' :: List Many a -> a
head' (Cons x _) = x

example1 :: Int
example1 = head' (1 `Cons` (2 `Cons` Nil))
