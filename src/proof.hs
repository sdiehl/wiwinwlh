{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}

data Z
data S n

data SNat n where
  Zero :: SNat Z
  Succ :: SNat n -> SNat (S n)

data Eql a b where
  Refl :: Eql a a

type family Add m n
type instance Add Z n = n
type instance Add (S m) n = S (Add m n)

add :: SNat n -> SNat m -> SNat (Add n m)
add Zero     m = m
add (Succ n) m = Succ (add n m)

cong :: Eql a b -> Eql (f a) (f b)
cong Refl = Refl

-- ∀n. 0 + suc n = suc n
plus_suc :: forall n.  SNat n
         -> Eql (Add Z (S n)) (S n)
plus_suc Zero = Refl
plus_suc (Succ n) = cong (plus_suc n)

-- ∀n. 0 + n = n
plus_zero :: forall n. SNat n
         -> Eql (Add Z n) n
plus_zero Zero = Refl
plus_zero (Succ n) = cong (plus_zero n)
