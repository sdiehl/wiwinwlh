import Control.Monad.Writer

type MyWriter = Writer [Int] Int

example :: MyWriter
example  = do
  tell [1..5]
  tell [5..10]
  return 3

output :: (Int, [Int])
output = runWriter example
