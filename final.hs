class Expr rep where
  lam :: (rep a -> rep b) -> rep (a -> b)
  app :: rep (a -> b) -> (rep a -> rep b)
  lit :: a -> rep a

newtype Interpret a = R { run :: a }

instance Expr Interpret where
  lam f   = R $ run . f . R
  app f a = R $ run f $ run a
  lit     = R

test1 :: Expr rep => rep Int
test1 = app (lam (\x -> x)) (lit 3)

test2 :: Expr rep => rep Int
test2 = app (lam (\x -> lit 4)) (lam $ \x -> lam $ \y -> y)

main :: IO ()
main = do
  print $ run test1
  print $ run test2
