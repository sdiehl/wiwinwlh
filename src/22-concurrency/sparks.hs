import Control.Parallel.Strategies hiding (parPair)

f :: Integer -> Integer
f x = x + 1

example :: Integer -> Integer -> Eval (Integer, Integer)
example x y = do
  a <- rpar $ f x
  b <- rpar $ f y
  rseq a
  rseq b
  return (a, b)

result :: (Integer, Integer)
result = runEval $ example 10 20
