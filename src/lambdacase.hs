{-# LANGUAGE LambdaCase #-}

data Exp a
  = Lam a (Exp a)
  | Var a
  | App (Exp a) (Exp a)

example :: Exp a -> a
example = \case
  Lam a b -> a
  Var a   -> a
  App a b -> example a
