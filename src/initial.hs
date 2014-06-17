{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

newtype Fix f = Fix { unFix :: f (Fix f) }

-- catamorphism
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- anamorphism
ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

-- hylomorphism
hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo f g = cata f . ana g

type Nat = Fix NatF
data NatF a = S a | Z deriving (Eq,Show)

instance Functor NatF where
  fmap f Z     = Z
  fmap f (S x) = S (f x)

plus :: Nat -> Nat -> Nat
plus n = cata phi where
  phi Z     = n
  phi (S m) = s m

times :: Nat -> Nat -> Nat
times n = cata phi where
  phi Z     = z
  phi (S m) = plus n m

int :: Nat -> Int
int = cata phi where
  phi  Z    = 0
  phi (S f) = 1 + f

nat :: Integer -> Nat
nat = ana (psi Z S) where
  psi f _ 0 = f
  psi _ f n = f (n-1)

z :: Nat
z = Fix Z

s :: Nat -> Nat
s = Fix . S


type Str = Fix StrF
data StrF x = Cons Char x | Nil

instance Functor StrF where
  fmap f (Cons a as) = Cons a (f as)
  fmap f Nil = Nil

nil :: Str
nil = Fix Nil

cons :: Char -> Str -> Str
cons x xs = Fix (Cons x xs)

str :: Str -> String
str = cata phi where
  phi Nil         = []
  phi (Cons x xs) = x : xs

str' :: String -> Str
str' = ana (psi Nil Cons) where
  psi f _ []     = f
  psi _ f (a:as) = f a as

map' :: (Char -> Char) -> Str -> Str
map' f = hylo g unFix
  where
    g Nil        = Fix Nil
    g (Cons a x) = Fix $ Cons (f a) x


type Tree a = Fix (TreeF a)
data TreeF a f = Leaf a | Tree a f f deriving (Show)

instance Functor (TreeF a) where
  fmap f (Leaf a) = Leaf a
  fmap f (Tree a b c) = Tree a (f b) (f c)

depth :: Tree a -> Int
depth = cata phi where
  phi (Leaf _)     = 0
  phi (Tree _ l r) = 1 + max l r


example1 :: Int
example1 = int (plus (nat 125) (nat 25))
-- 150
