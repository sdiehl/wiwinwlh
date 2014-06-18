import Control.Monad.Reader

type Env = [(String, Int)]
type Eval a = ReaderT Env Maybe a

data Expr
  = Val Int
  | Add Expr Expr
  | Var String
  deriving (Show)

eval :: Expr -> Eval Int
eval ex = case ex of

  Val n -> return n

  Add x y -> do
    a <- eval x
    b <- eval y
    return (a+b)

  Var x -> do
    env <- ask
    val <- lift (lookup x env)
    return val

env :: Env
env = [("x", 2), ("y", 5)]

ex1 :: Eval Int
ex1 = eval (Add (Val 2) (Add (Val 1) (Var "x")))

example1, example2 :: Maybe Int
example1 = runReaderT ex1 env
example2 = runReaderT ex1 []
