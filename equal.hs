{-# LANGUAGE GADTs #-}

data Eql a b where
  Refl :: Eql a a

sym :: Eql a b -> Eql b a
sym Refl = Refl

cong :: Eql a b -> Eql (f a) (f b)
cong Refl = Refl

trans :: Eql a b -> Eql b c -> Eql a c
trans Refl Refl = Refl

cast :: Eql a b -> a -> b
cast Refl = id
