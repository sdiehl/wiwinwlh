{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

import Data.Functor.Foldable

type Var = String

data Exp
  = Var Var
  | App Exp Exp
  | Lam [Var] Exp
  deriving Show

data ExpF a
  = VarF Var
  | AppF a a
  | LamF [Var] a
  deriving Functor

type instance Base Exp = ExpF

instance Foldable Exp where
  project (Var a)     = VarF a
  project (App a b)   = AppF a b
  project (Lam a b)   = LamF a b

instance Unfoldable Exp where
  embed (VarF a)      = Var a
  embed (AppF a b)    = App a b
  embed (LamF a b)    = Lam a b

fvs :: Exp -> [Var]
fvs = cata phi
  where phi (VarF a)    = [a]
        phi (AppF a b)  = a ++ b
        phi (LamF a b) = foldr (filter . (/=)) a b
