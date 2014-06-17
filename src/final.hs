import Prelude hiding (id)

class Expr rep where
  lam :: (rep a -> rep b) -> rep (a -> b)
  app :: rep (a -> b) -> (rep a -> rep b)
  lit :: a -> rep a

newtype Interpret a = R { reify :: a }

instance Expr Interpret where
  lam f   = R $ reify . f . R
  app f a = R $ reify f $ reify a
  lit     = R

eval :: Interpret a -> a
eval e = reify e

e1 :: Expr rep => rep Int
e1 = app (lam (\x -> x)) (lit 3)

e2 :: Expr rep => rep Int
e2 = app (lam (\x -> lit 4)) (lam $ \x -> lam $ \y -> y)

example1 :: Int
example1 = eval e1
-- 3

example2 :: Int
example2 = eval e2
-- 4
