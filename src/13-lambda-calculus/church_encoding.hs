{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Prelude hiding (not, succ, pred, fst, snd, tail, head)

type CBool = forall a. a -> a -> a

-- Booleans
true, false :: CBool
true x y  = x
false x y = y

-- Logic
not p      = p false true
and p q    = p q false
or p q     = p true q
cond p x y = p x y
xor p q    = p (not q) q

-- Tuples
fst p      = p true
snd p      = p false
pair a b f = f a b

-- Combinators
i x = x
k x y = x
s x y z = x z (y z)

b x y z = x (y z)
c x y z = x z y
w x y = x y y

-- Church Arithmetic
iszero n = n (\x -> false) true
succ n f x = f (n f x)

plus m n f x = n f (m f x)
sub m n = (n pred) m

mult m n f = m (n f)
pow m n = n m
pred n f x = n (\g h -> h (g f)) (\u -> x) i

leq m n = iszero (sub m n)
geq m n = not (leq m n)

-- Church Numbers
type CNat = forall a. (a -> a) -> a -> a

zero, one, two, three :: CNat
zero  f x = x
one   f x = f x
two   f x = f (f x)
three f x = f (f (f x))

-- Scott Lists (lists as nested tuples)
nil z      = z
cons x y   = pair false (pair x y)
null z     = z true
head z     = fst (snd z)
tail z     = snd (snd z)
index xs n = head (n tail xs)

-- data Nat = Z | S Nat
ezero   = \s z -> z
esucc n = \s z -> s (n s z)

-- data Expr = Lam Expr | App Expr Expr | Var Int
elam f   = \l a v -> l f
eapp t u = \l a v -> a t u
evar n   = \l a v -> v n

-- Convert between Ints and Church Numbers
unchurch :: CNat -> Integer
unchurch n = n (\i -> i + 1) 0

church :: Int -> CNat
church n =
  if n == 0
  then zero
  else \f x -> f (church (n-1) f x)

unbool :: (Bool -> Bool -> t) -> t
unbool n = n True False



ex1 :: Integer
ex1 = unchurch (pow three three)
-- 27

ex2 :: Bool
ex2 = unbool (iszero (pred one))
-- True

ex3 :: Integer
ex3 = snd (pair 1 2)
-- 2

ex4 :: Integer
ex4 = head (tail (cons 1 (cons 2 nil)))
-- 2

ex5 :: Bool
ex5 = unbool (true `xor` false)
-- True
