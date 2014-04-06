import Test.SmallCheck

assoc :: Int -> Int -> Int -> Bool
assoc a b c = a * (b + c) == a * b + a *c

cauchy :: [Double] -> [Double] -> Bool
cauchy xs ys = (abs (dot xs ys))^2 <= (dot xs xs) * (dot ys ys)

failure :: [Double] -> [Double] -> Bool
failure xs ys = (abs (dot xs ys)) <= (dot xs xs) * (dot ys ys)

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)

main :: IO ()
main = do
  putStrLn "Testing associativity..."
  smallCheck 25 assoc

  putStrLn "Testing Cauchy-Swartz..."
  smallCheck 4 cauchy

  putStrLn "Testing invalid Cauchy-Swartz..."
  smallCheck 4 failure
