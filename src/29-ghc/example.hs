f :: Int -> Int
f x = x + 1

main :: IO ()
main = do
  print (f 10)
