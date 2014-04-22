{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative
import qualified Data.Map as M

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo f g = cata f . ana g

type Id = String
type Env = M.Map Id Int

type Expr = Fix ExprF
data ExprF a
  = Lit Int
  | Var Id
  | Add a a
  | Mul a a
  deriving (Show, Eq, Ord, Functor)

deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Ord (f (Fix f)) => Ord (Fix f)
deriving instance Show (f (Fix f)) => Show (Fix f)

eval :: M.Map Id Int -> Fix ExprF -> Maybe Int
eval env = cata phi where
  phi ex = case ex of
    Lit c   -> pure c
    Var i   -> M.lookup i env
    Add x y -> liftA2 (+) x y
    Mul x y -> liftA2 (*) x y

expr :: Expr
expr = Fix (Mul n (Fix (Add x y)))
  where
    n = Fix (Lit 10)
    x = Fix (Var "x")
    y = Fix (Var "y")

env :: M.Map Id Int
env = M.fromList [("x", 1), ("y", 2)]

compose :: (f (Fix f) -> c) -> (a -> Fix f) -> a -> c
compose x y = x . unFix . y

example :: Maybe Int
example = eval env expr
-- Just 30
