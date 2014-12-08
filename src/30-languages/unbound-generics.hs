{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LC where

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

import GHC.Generics

import Data.Typeable (Typeable)
import Data.Set as S

import Control.Monad.Reader (Reader, runReader)

data Exp
  = Var (Name Exp)
  | Lam (Bind (Name Exp) Exp)
  | App Exp Exp
  deriving (Show, Generic, Typeable)

instance Alpha Exp

instance Subst Exp Exp where
  isvar (Var x) = Just (SubstName x)
  isvar _       = Nothing

fvSet :: (Alpha a, Typeable b) => a -> S.Set (Name b)
fvSet = S.fromList . toListOf fv

type M a = FreshM a

(=~) :: Exp -> Exp -> M Bool
e1 =~ e2 | e1 `aeq` e2 = return True
e1 =~ e2 = do
    e1' <- red e1
    e2' <- red e2
    if e1' `aeq` e1 && e2' `aeq` e2
      then return False
      else e1' =~ e2'

-- Reduction
red :: Exp -> M Exp
red (App e1 e2) = do
  e1' <- red e1
  e2' <- red e2
  case e1' of
    Lam bnd -> do
        (x, e1'') <- unbind bnd
        return $ subst x e2' e1''
    otherwise -> return $ App e1' e2'
red (Lam bnd) = do
   (x, e) <- unbind bnd
   e' <- red e
   case e of
     App e1 (Var y) | y == x && x `S.notMember` fvSet e1 -> return e1
     otherwise -> return (Lam (bind x e'))
red (Var x) = return $ (Var x)


x :: Name Exp
x = string2Name "x"

y :: Name Exp
y = string2Name "y"

z :: Name Exp
z = string2Name "z"

s :: Name Exp
s = string2Name "s"

lam :: Name Exp -> Exp -> Exp
lam x y = Lam (bind x y)

zero  = lam s (lam z (Var z))
one   = lam s (lam z (App (Var s) (Var z)))
two   = lam s (lam z (App (Var s) (App (Var s) (Var z))))
three = lam s (lam z (App (Var s) (App (Var s) (App (Var s) (Var z)))))

plus = lam x (lam y (lam s (lam z (App (App (Var x) (Var s)) (App (App (Var y) (Var s)) (Var z))))))

true = lam x (lam y (Var x))
false = lam x (lam y (Var y))
if_ x y z = (App (App x y) z)

main :: IO ()
main = do
  print $ lam x (Var x) `aeq` lam y (Var y)
  print $ not (lam x (Var y) `aeq` lam x (Var x))
  print $ lam x (App (lam y (Var x)) (lam y (Var y))) =~ (lam y (Var y))
  print $ lam x (App (Var y) (Var x)) =~ Var y
  print $ if_ true (Var x) (Var y) =~ Var x
  print $ if_ false (Var x) (Var y) =~ Var y
  print $ App (App plus one) two =~ three
