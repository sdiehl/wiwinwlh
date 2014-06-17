import qualified Data.Vector as V

data Order = RowMajor | ColMajor

rowMajor :: [Int] -> [Int]
rowMajor = scanr (*) 1 . tail

colMajor :: [Int] -> [Int]
colMajor = init . scanl (*) 1

data Matrix a = Matrix
  { _dims  :: [Int]
  , _elts  :: V.Vector a
  , _order :: Order
  }

fromList :: [Int] -> Order -> [a] -> Matrix a
fromList sh order elts =
  if product sh == length elts
  then Matrix sh (V.fromList elts) order
  else error "dimensions don't match"

indexTo :: [Int] -> Matrix a -> a
indexTo ix mat = boundsCheck offset
  where
    boundsCheck n =
      if 0 <= n && n < V.length (_elts mat)
      then V.unsafeIndex (_elts mat) offset
      else error "out of bounds"
    ordering = case _order mat of
      RowMajor -> rowMajor
      ColMajor -> colMajor
    offset = sum $ zipWith (*) ix (ordering (_dims mat))

matrix :: Order -> Matrix Int
matrix order = fromList [4,4] order [1..16]

ex1 :: [Int]
ex1 = rowMajor [1,2,3,4]
-- [24,12,4,1]

ex2 :: [Int]
ex2 = colMajor [1,2,3,4]
-- [1,1,2,6]

ex3 :: Int
ex3 = indexTo [1,3] (matrix RowMajor)
-- 8

ex4 :: Int
ex4 = indexTo [1,3] (matrix ColMajor)
-- 14
