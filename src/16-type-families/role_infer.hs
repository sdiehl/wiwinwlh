{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}

data Nat = Zero | Suc Nat

type role Vec nominal representational
data Vec :: Nat -> * -> * where
  Nil  :: Vec Zero a
  (:*) :: a -> Vec n a -> Vec (Suc n) a

type role App representational nominal
data App (f :: k -> *) (a :: k) = App (f a)

type role Mu nominal nominal
data Mu (f :: (k -> *) -> k -> *) (a :: k) = Roll (f (Mu f) a)

type role Proxy phantom
data Proxy (a :: k) = Proxy
