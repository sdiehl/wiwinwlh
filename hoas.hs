{-# LANGUAGE GADTs #-}

data Expr a where
  Con :: a -> Expr a
  Lam :: (Expr a -> Expr b) -> Expr (a -> b)
  App :: Expr (a -> b) -> Expr a -> Expr b

eval :: Expr a -> a
eval (Con v) = v
eval (Lam f) = \x -> eval (f (Con x))
eval (App e1 e2) = (eval e1) (eval e2)

subst :: a -> b -> Expr a -> Expr a
subst x y e = let x = y in e
