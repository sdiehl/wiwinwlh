import Control.Monad.Reader

data Expr
  = Val Int
  | Add Expr Expr
  | Var String
  deriving (Show)

type Env = [(String, Int)]
type Eval a = ReaderT Env Maybe a

eval :: Expr -> Eval Int
eval (Val n) = return n
eval (Add x y) = liftM2 (+) (eval x) (eval y)
eval (Var x) = do
  env <- ask
  val <- lift (lookup x env)
  return val


ex :: Eval Int
ex = eval (Add (Val 2) (Add (Val 1) (Var "x")))

env :: Env
env = [("x", 2), ("y", 5)]

example1, example2 :: Maybe Int
example1 = runReaderT ex env
example2 = runReaderT ex []
