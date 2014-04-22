import Test.QuickCheck

qsort :: [Int] -> [Int]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

prop_maximum ::  [Int] -> Property
prop_maximum xs = not (null xs) ==>
                  last (qsort xs) == maximum xs

main :: IO ()
main = quickCheck prop_maximum
