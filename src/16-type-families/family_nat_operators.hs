{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

data Z
data S n

data Nat n where
  Zero :: Nat Z
  Succ :: Nat n -> Nat (S n)

data a :=: b where
  Refl :: a :=: a

type family Add m n
type instance Add Z n = n
type instance Add (S m) n = S (Add m n)

type family Pred n
type instance Pred Z = Z
type instance Pred (S n) = n

add :: Nat n -> Nat m -> Nat (Add n m)
add Zero     m = m
add (Succ n) m = Succ (add n m)

cong :: a :=: b -> (f a) :=: (f b)
cong Refl = Refl

plus_zero :: forall n. Nat n -> (Add n Z) :=: n
plus_zero Zero = Refl
plus_zero (Succ n) = cong (plus_zero n)

type family m :+ n :: *
type instance Z :+ n = n
type instance (S m) :+ n = S (m :+ n)

-- m+1+n = 1+m+n
assoc :: forall m n. Nat m -> Nat n -> (m :+ (S Z) :+ n) :=: (S Z :+ m :+ n)
assoc Zero n = Refl
assoc (Succ m) n = cong (assoc m n)
