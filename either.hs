sdiv :: Double -> Double -> Either String Double
sdiv _ 0 = throwError "divide by zero"
sdiv i j = return $ i / j

example :: Double -> Double -> Either String Double
example n m = do
  a <- sdiv n m
  b <- sdiv 2 a
  c <- sdiv 2 b
  return c

throwError :: String -> Either String b
throwError a = Left a

main :: IO ()
main = do
  print $ example 1 5
  print $ example 1 0
