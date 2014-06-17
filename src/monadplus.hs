import Safe
import Control.Monad

list1 :: [(Int,Int)]
list1 = [(a,b) | a <- [1..25], b <- [1..25], a < b]

list2 :: [(Int,Int)]
list2 = do
  a <- [1..25]
  b <- [1..25]
  guard (a < b)
  return $ (a,b)

maybe1 :: String -> String -> Maybe Double
maybe1 a b = do
  a' <- readMay a
  b' <- readMay b
  guard (b' /= 0.0)
  return $ a'/b'

maybe2 :: Maybe Int
maybe2 = msum [Nothing, Nothing, Just 3, Just 4]
