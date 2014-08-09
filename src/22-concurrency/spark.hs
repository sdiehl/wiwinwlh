import Control.Parallel.Strategies

parMap' :: (a -> b) -> [a] -> Eval [b]
parMap' f [] = return []
parMap' f (a:as) = do
  b  <- rpar (f a)
  bs <- parMap' f as
  return (b:bs)

result :: [Int]
result = runEval $ parMap' (+1) [1..1000]
