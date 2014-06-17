{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Proxy
import GHC.TypeLits
import Data.Type.Equality

type family Z :: Nat where
  Z = 0

type family S (n :: Nat) :: Nat where
  S n = n + 1

eq_one :: 1 :~: 1
eq_one = Refl

eq_one_one :: 1 + 1 :~: 2
eq_one_one = Refl

cong :: a :~: b -> f a :~: f b
cong Refl = Refl

subst :: a :~: b -> f a -> f b
subst Refl = id

plus_zero :: forall n. (n + Z) :~: n
plus_zero = Refl

plus_one :: forall n. (n + S Z) :~: S n
plus_one = Refl

-- No.
-- plus_comm :: forall n m. (n + m) :~: (m + n)
-- plus_comm = Refl

-- No.
-- plus_suc :: forall n m. (n + (S m)) :~: (S (n + m))
-- plus_suc = Refl

-- No.
-- nontrivial :: forall n. (n + 1) :~: (1 + n)
-- nontrivial = Refl
