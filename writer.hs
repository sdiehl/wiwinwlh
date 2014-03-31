import Control.Monad.Writer

type MyWriter = Writer [Int] Int

example :: MyWriter
example  = do
  tell [1]
  tell [1..25]
  return 3

{-
execWriter :: Writer w a -> w
runWriter :: Writer w a -> (a, w)
-}

output :: (Int, [Int])
output = runWriter example
