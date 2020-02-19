import Control.Monad.Except
import Control.Monad.Identity

data Exception
  = Failure String
  | GenericFailure
  deriving (Show)

type ErrMonad a = ExceptT Exception Identity a

example :: Int -> Int -> ErrMonad Int
example x y = do
  case y of
    0 -> throwError $ Failure "division by zero"
    x -> return $ x `div` y

runFail :: ErrMonad a -> Either Exception a
runFail = runIdentity . runExceptT

example1 :: Either Exception Int
example1 = runFail $ example 2 3

example2 :: Either Exception Int
example2 = runFail $ example 2 0
