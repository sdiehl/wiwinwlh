{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

data Size = Empty | NonEmpty

data List a b where
  Nil  :: List Empty a
  Cons :: a -> List b a -> List NonEmpty a

head' :: List NonEmpty a -> a
head' (Cons x _) = x

example1 :: Int
example1 = head' (1 `Cons` (2 `Cons` Nil))

-- Cannot match type Empty with NonEmpty
example2 :: Int
example2 = head' Nil
