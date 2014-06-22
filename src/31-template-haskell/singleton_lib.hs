{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

import Data.Singletons
import Data.Singletons.TH

$(singletons [d|
  data Nat = Zero | Succ Nat
    deriving (Eq, Show)

  plus :: Nat -> Nat -> Nat
  plus Zero     n = n
  plus (Succ m) n = Succ (plus m n)

  isEven :: Nat -> Bool
  isEven Zero = True
  isEven (Succ Zero) = False
  isEven (Succ (Succ n)) = isEven n
  |])
