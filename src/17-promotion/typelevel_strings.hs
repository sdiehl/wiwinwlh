{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import GHC.TypeLits

data Tagged (l :: Symbol) a = Tag a
  deriving (Show)

m :: Tagged "m" Double
m = Tag 10.0

s :: Tagged "s" Double
s = Tag 20.0

divUnits ::
  Fractional a =>
  Tagged u1 a ->
  Tagged u2 a ->
  Tagged (u1 `AppendSymbol` u2) a
divUnits (Tag x) (Tag y) = Tag (x / y)

addUnits ::
  (Num a, u1 `CmpSymbol` u2 ~ 'EQ) =>
  Tagged u1 a ->
  Tagged u2 a ->
  Tagged u1 a
addUnits (Tag x) (Tag y) = Tag (x + y)
