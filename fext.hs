{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

class Expr repr where
  lit :: Int -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr
  mul :: repr -> repr -> repr

instance Expr Int where
  lit n = n
  neg a = -a
  add a b = a + b
  mul a b = a * b

instance Expr String where
  lit n = show n
  neg a = "(-" ++ a ++ ")"
  add a b = "(" ++ a ++ " + " ++ b ++ ")"
  mul a b = "(" ++ a ++ " * " ++ b ++ ")"

class BoolExpr repr where
  eq :: repr -> repr -> repr
  tr :: repr
  fl :: repr

instance BoolExpr Int where
  eq a b = if a == b then tr else fl
  tr = 1
  fl = 0

instance BoolExpr String where
  eq a b = "(" ++ a ++ " == " ++ b ++ ")"
  tr = "true"
  fl = "false"

eval :: Int -> Int
eval = id

render :: String -> String
render = id

expr :: (BoolExpr repr, Expr repr) => repr
expr = eq (add (lit 1) (lit 2)) (lit 3)

result :: Int
result = eval expr
-- 1

string :: String
string = render expr
-- "((1 + 2) == 3)"
