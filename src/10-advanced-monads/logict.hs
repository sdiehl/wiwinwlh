import Control.Monad

range :: MonadPlus m => [a] -> m a
range [] = mzero
range (x:xs) = range xs `mplus` return x

pyth :: Integer -> [(Integer,Integer,Integer)]
pyth n = do
  x <- range [1..n]
  y <- range [1..n]
  z <- range [1..n]
  if x*x + y*y == z*z then return (x,y,z) else mzero

main :: IO ()
main = print $ pyth 15
{-
[ ( 12 , 9 , 15 )
, ( 12 , 5 , 13 )
, ( 9 , 12 , 15 )
, ( 8 , 6 , 10 )
, ( 6 , 8 , 10 )
, ( 5 , 12 , 13 )
, ( 4 , 3 , 5 )
, ( 3 , 4 , 5 )
]
-}
