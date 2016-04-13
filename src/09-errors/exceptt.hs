import Control.Monad.Except

data Failure
  = NonPositive Int
  | ReadError String
  deriving Show

example :: Int -> Int -> Except Failure Int
example a b = do
  if b == 0
  then throwError (NonPositive b)
  else return (a `div` b)

runExample :: IO ()
runExample = do
  print ((runExcept (example 1 2)) :: Either Failure Int)
  print ((runExcept (example 1 0)) :: Either Failure Int)
