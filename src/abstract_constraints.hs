{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

import Data.List
import Data.Type.Equality
import GHC.Exts (Constraint)

type C1 a = (Num a, Ord a)
type C2 a = (C1 a, Fractional a)

f :: C1 a => a -> a -> [a]
f a b = sort [(a+b)^n | n <- [1..25]]

g :: C2 a => a -> a -> [a]
g a b = sort [(a+b)^n | n <- [1..25]]

-- Translate propositional equality proof as constraint over type variables.
type EqP a b r = ((a ~ b) => r)

gcastWith' :: (a :~: b) -> EqP a b r -> r
gcastWith' Refl x = x

castWith' :: (a :~: b) -> a -> b
castWith' Refl x = gcastWith' Refl x
