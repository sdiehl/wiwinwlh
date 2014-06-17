{-# LANGUAGE RankNTypes #-}

newtype List a = List (forall b. (a -> b -> b) -> b -> b)

fromList :: [a] -> List a
fromList xs = List (\n c -> foldr n c xs)

toList :: List a -> [a]
toList xs = unList xs (:) []

unList :: List a
        -> (a -> b -> b) -- Cons
        -> b             -- Nil
        -> b
unList (List l) = l

nil :: List a
nil = List (\n c -> c)

cons :: a -> List a -> List a
cons x xs = List (\n c -> n x (unList xs n c))

append :: List a -> List a -> List a
append xs ys = List (\n c -> unList xs n (unList ys n c))

singleton :: a -> List a
singleton x = List (\n c -> n x c)

length :: List a -> Integer
length (List l) = l (\_ n -> n + 1) 0

test :: [Integer]
test = toList (fromList [1,2,3] `append` fromList [4,5,6])
