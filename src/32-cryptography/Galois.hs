{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}

module Galois where

import Data.Field.Galois
import Prelude hiding ((/))

-- Prime field
type Fq = Prime 2147483647

exampleFq :: IO ()
exampleFq = do
  print ((1 + 0x7FFFFFFF16) :: Fq)
  print ((10000 * 10000) :: Fq)
  print ((1 / 524287) :: Fq)

-- Polynomial term
data P2

-- Extension field
type Fq2 = Extension P2 Fq

-- Irreducublie monic polynomial extension
instance IrreducibleMonic P2 Fq where
  poly _ = X2 + 1

-- Polynomial 2*x^2 + 1 over Fq2
p1 :: Fq2
p1 = [1, 2]

p2 :: Fq2
p2 = (p1 + p1) * 2

p3 :: Bool
p3 = p2 / p1 == 4
