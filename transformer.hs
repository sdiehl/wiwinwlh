import Control.Monad.Reader

data Expr
  = Val Int
  | Add Expr Expr
  | Var String
  deriving (Show)

type Env = [(String, Int)]

eval :: Expr -> ReaderT Env Maybe Int
eval (Val n) = return n
eval (Add x y) = liftM2 (+) (eval x) (eval y)
eval (Var x) = do
  env <- ask
  val <- lift (lookup x env)
  return val

ex ::  ReaderT Env Maybe Int
ex = eval (Add (Val 2) (Add (Val 1) (Var "x")))

main ::  IO ()
main = do
    print $ runReaderT ex [("x", 2)]
    print $ runReaderT ex []
