{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Test.SmallCheck
import Test.SmallCheck.Series
import Control.Applicative

import qualified Data.Vector as V

dot :: Num a => V.Vector a -> V.Vector a -> a
dot xs ys = V.sum (V.zipWith (*) xs ys)

cauchy :: V.Vector Double -> V.Vector Double -> Bool
cauchy xs ys = (abs (dot xs ys))^2 <= (dot xs xs) * (dot ys ys)

instance (Serial m a, Monad m) => Serial m (V.Vector a) where
  series = V.fromList <$> series

main :: IO ()
main = smallCheck 4 cauchy
