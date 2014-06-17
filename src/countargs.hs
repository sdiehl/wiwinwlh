{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Proxy
import GHC.TypeLits

type family Count (f :: *) :: Nat where
  Count (a -> b) = 1 + (Count b)
  Count x = 1

type Fn1 = Int -> Int
type Fn2 = Int -> Int -> Int -> Int

fn1 :: Integer
fn1 = natVal (Proxy :: Proxy (Count Fn1))
-- 2

fn2 :: Integer
fn2 = natVal (Proxy :: Proxy (Count Fn2))
-- 4
