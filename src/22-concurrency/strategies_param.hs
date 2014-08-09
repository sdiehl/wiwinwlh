import Control.DeepSeq
import Control.Parallel.Strategies

evalPair :: Strategy a -> Strategy b -> Strategy (a, b)
evalPair sa sb (a, b) = do
  a' <- sa a
  b' <- sb b
  return (a', b')

parPair :: Strategy a -> Strategy b -> Strategy (a, b)
parPair sa sb = evalPair (rparWith sa) (rparWith sb)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

serial :: ([Int], [Int])
serial = (a, b)
  where
    a = fmap fib [0..30]
    b = fmap fib [1..30]

parallel :: ([Int], [Int])
parallel = (a, b) `using` evalPair rdeepseq rdeepseq
  where
    a = fmap fib [0..30]
    b = fmap fib [1..30]
