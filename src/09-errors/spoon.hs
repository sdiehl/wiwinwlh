import Control.Spoon

goBoom :: Int -> Int -> Int
goBoom x y = x `div` y

-- evaluate to normal form
test1 :: Maybe [Int]
test1 = spoon [1, 2, undefined]

-- evaluate to weak head normal form
test2 :: Maybe [Int]
test2 = teaspoon [1, 2, undefined]

main :: IO ()
main = do
  maybe (putStrLn "Nothing") (print . length) test1
  maybe (putStrLn "Nothing") (print . length) test2
