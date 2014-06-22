{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}

-- a ≡ b
data a :~: b where
  Refl :: a :~: a

-- (f : A → B) {x y} → x ≡ y → f x ≡ f y
cong :: a :~: b -> (f a) :~: (f b)
cong Refl = Refl

-- {a b : A} → a ≡ b → a ≡ b
sym :: a :~: b -> b :~: a
sym Refl = Refl

-- {a b c : A} → a ≡ b → b ≡ c → a ≡ c
trans :: a :~: b -> b :~: c -> a :~: c
trans Refl Refl = Refl

-- {a b : A} → a ≡ b → a → b
cast :: a :~: b -> a -> b
cast Refl = id


a :: forall n. n :~: n
a = Refl

b :: forall n. (Maybe n) :~: (Maybe n)
b = Refl

c :: forall. Eql () :~: ()
c = Refl
