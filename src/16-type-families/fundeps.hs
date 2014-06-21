{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

data Z
data S n

type Zero  = Z
type One   = S Zero
type Two   = S One
type Three = S Two
type Four  = S Three

zero :: Zero
zero = undefined

one :: One
one = undefined

two :: Two
two = undefined

three :: Three
three = undefined

four :: Four
four = undefined

class Eval a where
  eval :: a -> Int

instance Eval Zero where
  eval _ = 0

instance Eval n => Eval (S n) where
  eval m = 1 + eval (prev m)

class Pred a b | a -> b where
  prev :: a -> b

instance Pred Zero Zero where
  prev = undefined

instance Pred (S n) n where
  prev = undefined

class Add a b c | a b -> c where
  add :: a -> b -> c

instance Add Zero a a where
  add = undefined

instance Add a b c => Add (S a) b (S c) where
  add = undefined

f :: Three
f = add one two

g :: S (S (S (S Z)))
g = add two two

h :: Int
h = eval (add three four)
