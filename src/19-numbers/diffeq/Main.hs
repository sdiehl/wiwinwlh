{-# LANGUAGE OverloadedLists #-}

module Main where

import Numeric.GSL.ODE
import Numeric.LinearAlgebra

-- Differential equation
f :: Double -> [Double] -> [Double]
f t [x, v] = [v, - x + mu * v * (1 - x ^ 2)]

-- Mu scalar, dampening strenth
mu :: Double
mu = 0.1

-- Boundary conditions
ts :: Vector Double
ts = linspace 1000 (0, 50)

-- Use default solver: Embedded Runge-Kutta-Fehlberg (4, 5) method.
vanderpol1 :: [Vector Double]
vanderpol1 = toColumns $ odeSolve f [1, 0] ts

-- Use Runge-Kutta (2,3) solver
vanderpol2 :: [Vector Double]
vanderpol2 = toColumns $ odeSolveV RK2 hi epsAbs epsRel (l2v f) [1, 0] ts
  where
    epsAbs = 1.49012e-08
    epsRel = epsAbs
    hi = (ts ! 1 - ts ! 0) / 100
    l2v f = \t -> fromList . f t . toList

main :: IO ()
main = do
  print vanderpol1
  print vanderpol2
