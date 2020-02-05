{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

import Control.Applicative
import Data.Functor.Const (Const (..))
import GHC.Exts (Any)

-- Deriving Eq in terms of Const functor
newtype Age = MkAge Int
  deriving
    (Eq)
    via Const Int Any

-- Deriving Num across a nested functor
newtype FNum f a = FNum (f a)
  deriving stock (Functor)
  deriving newtype (Applicative)

instance (Applicative f, Num a) => Num (FNum f a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = FNum . pure . fromInteger

newtype Example a b = Example (Either a b)
  deriving stock (Show, Functor)
  deriving newtype (Applicative)
  deriving (Num) via FNum (Either a) b

a :: Example Integer Integer
a = Example (Left 1)

b :: Example Integer Integer
b = Example (Right 1)

example :: IO ()
example = do
  print (a + a)
  print (a + b)
  print (b + b)
