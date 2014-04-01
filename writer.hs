import Control.Monad.Writer

type MyWriter = Writer [Int] Int

example :: MyWriter
example  = do
  tell [1]
  tell [1..25]
  return 3

output :: (Int, [Int])
output = runWriter example
