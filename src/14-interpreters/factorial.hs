import Data.Functor.Foldable

factorial :: Int -> Int
factorial = hylo alg coalg
  where
    coalg :: Int -> ListF Int Int
    coalg m
      | m <= 1 = Nil
      | otherwise = Cons m (m - 1)
    alg :: ListF Int Int -> Int
    alg Nil = 1
    alg (Cons a x) = a * x
