{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}

data Z
data S n

data Nat n where
  Zero :: Nat Z
  Succ :: Nat n -> Nat (S n)

data Eql a b where
  Refl :: Eql a a

type family Add m n
type instance Add Z n = n
type instance Add (S m) n = S (Add m n)

type family Pred n
type instance Pred Z = Z
type instance Pred (S n) = n

add :: Nat n -> Nat m -> Nat (Add n m)
add Zero     m = m
add (Succ n) m = Succ (add n m)

cong :: Eql a b -> Eql (f a) (f b)
cong Refl = Refl

plus_zero :: forall n. Nat n -> Eql (Add n Z) n
plus_zero Zero = Refl
plus_zero (Succ n) = cong (plus_zero n)
