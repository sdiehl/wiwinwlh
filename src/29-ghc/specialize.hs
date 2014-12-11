module Specialize (spec, nonspec, f) where

{-# SPECIALIZE INLINE f :: Double -> Double -> Double #-}

f :: Floating a => a -> a -> a
f x y = exp (x + y) * exp (x + y)

nonspec :: Float
nonspec = f (10 :: Float) (20 :: Float)

spec :: Double
spec = f (10 :: Double) (20 :: Double)
