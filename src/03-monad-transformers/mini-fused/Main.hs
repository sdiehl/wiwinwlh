{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Carrier.Throw.Either
import Control.Exception
import Data.Functor.Identity

example1 :: Has (State Integer) sig m => m Integer
example1 = do
  modify (+ 1)
  modify (* 10)
  get

example2 ::
  ( Has (State (Double, Double)) sig m,
    Has (Throw ArithException) sig m
  ) =>
  m Double
example2 = do
  (a, b) <- get
  if b == 0
    then throwError DivideByZero
    else pure (a / b)

ex1 :: (Algebra sig m, Effect sig) => m Integer
ex1 = evalState (1 :: Integer) example1

run1 :: Identity Integer
run1 = runM ex1

run2 :: IO Integer
run2 = runM ex1

ex2 :: (Algebra sig m, Effect sig) => m (Either ArithException Double)
ex2 = runThrow $ evalState (1 :: Double, 2 :: Double) example2

ex3 :: (Algebra sig m, Effect sig) => m (Either ArithException Double)
ex3 = evalState (1 :: Double, 0 :: Double) (runThrow example2)

main :: IO ()
main = pure ()
