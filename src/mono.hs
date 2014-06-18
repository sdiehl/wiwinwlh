{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Data.Char
import Data.Monoid
import Data.MonoTraversable
import Control.Applicative

bs :: Text
bs = "Hello Haskell."

shift :: Text
shift = omap (chr . (+1) . ord) bs
-- "Ifmmp!Ibtlfmm/"

backwards :: [Char]
backwards = ofoldl' (flip (:)) "" bs
-- ".lleksaH olleH"


data MyMonoType = MNil | MCons Int MyMonoType deriving Show

type instance Element MyMonoType = Int

instance MonoFunctor MyMonoType where
  omap f MNil = MNil
  omap f (MCons x xs) = f x `MCons` omap f xs

instance MonoFoldable MyMonoType where
  ofoldMap f = ofoldr (mappend . f) mempty
  ofoldr       = mfoldr
  ofoldl'      = mfoldl'
  ofoldr1Ex f  = ofoldr1Ex f . mtoList
  ofoldl1Ex' f = ofoldl1Ex' f . mtoList

instance MonoTraversable MyMonoType where
  omapM f xs = mapM f (mtoList xs) >>= return . mfromList
  otraverse f = ofoldr acons (pure MNil)
    where acons x ys = MCons <$> f x <*> ys

mtoList :: MyMonoType -> [Int]
mtoList (MNil) = []
mtoList (MCons x xs) = x : (mtoList xs)

mfromList :: [Int] -> MyMonoType
mfromList [] = MNil
mfromList (x:xs) = MCons x (mfromList xs)

mfoldr :: (Int -> a -> a) -> a -> MyMonoType -> a
mfoldr f z MNil =  z
mfoldr f z (MCons x xs) =  f x (mfoldr f z xs)

mfoldl' :: (a -> Int -> a) -> a -> MyMonoType -> a
mfoldl' f z MNil = z
mfoldl' f z (MCons x xs) = let z' = z `f` x
                           in seq z' $ mfoldl' f z' xs

ex1 :: Int
ex1 = mfoldl' (+) 0 (mfromList [1..25])

ex2 :: MyMonoType
ex2 = omap (+1) (mfromList [1..25])
