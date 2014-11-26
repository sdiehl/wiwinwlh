{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}

-- a ≡ b
data Eql a b where
  Refl :: Eql a a

-- Congruence
-- (f : A → B) {x y} → x ≡ y → f x ≡ f y
cong :: Eql a b -> Eql (f a) (f b)
cong Refl = Refl

-- Symmetry
-- {a b : A} → a ≡ b → a ≡ b
sym :: Eql a b -> Eql b a
sym Refl = Refl

-- Transitivity
-- {a b c : A} → a ≡ b → b ≡ c → a ≡ c
trans :: Eql a b -> Eql b c -> Eql a c
trans Refl Refl = Refl

-- Coerce one type to another given a proof of their equality.
-- {a b : A} → a ≡ b → a → b
castWith :: Eql a b -> a -> b
castWith Refl = id

-- Trivial cases
a :: forall n. Eql n n
a = Refl

b :: forall. Eql () ()
b = Refl
