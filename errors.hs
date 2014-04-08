import Control.Monad.Error
import Control.Monad.Identity

data Exception
  = Failure String
  | GenericFailure
  deriving Show

instance Error Exception where
  noMsg = GenericFailure

type FailMonad a = ErrorT Exception Identity a

example :: Int -> Int -> FailMonad Int
example x y = do
  case y of
    0 -> throwError $ Failure "it didn't work!"
    x -> return $ x `div` y

runFail :: FailMonad a -> Either Exception a
runFail = runIdentity . runErrorT

example1 = runFail $ example 2 3
example2 = runFail $ example 2 0
