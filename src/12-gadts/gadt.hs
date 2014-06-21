{-# Language GADTs #-}

data Term a where
  Lit    :: a -> Term a
  Succ   :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If     :: Term Bool -> Term a -> Term a -> Term a

eval :: Term a -> a
eval (Lit i)      = i                                   -- Term a
eval (Succ t)     = 1 + eval t                          -- Term (a ~ Int)
eval (IsZero i)   = eval i == 0                         -- Term (a ~ Int)
eval (If b e1 e2) = if eval b then eval e1 else eval e2 -- Term (a ~ Bool)

example :: Int
example = eval (Succ (Succ (Lit 3)))
