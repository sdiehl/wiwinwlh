{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Free
import Control.Monad

import Data.Monoid

data Cons a b = Cons a b deriving (Eq, Show)

instance Functor (Cons a) where
  fmap f (Cons a b) = Cons a (f b)

type List a = Free (Cons a) ()

nil :: List a
nil = Pure ()

cons :: a -> List a -> List a
cons x xs = Free (Cons x xs)

append :: a -> List a
append x = cons x nil

example1 :: List Int
example1 = 1 `cons` (2 `cons` nil)

example2 :: List Int
example2 = append 1 >> append 2 >> append 3

example3 :: Free [] Int
example3 = liftF [1..5]

example4 :: [Int]
example4 = retract example3

example5 :: Free Maybe Int
example5 = liftF (Just 3)

example6 :: Maybe Int
example6 = iterM join example5

example7 :: [Int]
example7 = iterM join example3
