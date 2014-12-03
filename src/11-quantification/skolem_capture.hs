{-# LANGUAGE RankNTypes #-}

escape :: (forall a. a -> a) -> Int
escape f = f 0

g x = escape (\a -> x)
