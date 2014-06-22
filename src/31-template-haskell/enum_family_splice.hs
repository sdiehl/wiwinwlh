{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

import EnumFamily

import Data.Proxy
import GHC.TypeLits

type family Mod (m :: Nat) (n :: Nat) :: Nat
type family Add (m :: Nat) (n :: Nat) :: Nat
type family Pow (m :: Nat) (n :: Nat) :: Nat

enumFamily mod ''Mod 10
enumFamily (+) ''Add 10
enumFamily (^) ''Pow 10

a :: Integer
a = natVal (Proxy :: Proxy (Mod 6 4))
-- 2

b :: Integer
b = natVal (Proxy :: Proxy (Pow 3 (Mod 6 4)))
-- 9

--    enumFamily mod ''Mod 3
--  ======>
--    template_typelevel_splice.hs:7:1-14
--    type instance Mod 2 1 = 0
--    type instance Mod 2 2 = 0
--    type instance Mod 2 3 = 2
--    type instance Mod 3 1 = 0
--    type instance Mod 3 2 = 1
--    type instance Mod 3 3 = 0
--    ...
