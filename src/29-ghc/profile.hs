import qualified Data.Vector.Unboxed as U

data Pair = Pair {-# UNPACK #-}!Int {-# UNPACK #-}!Double

mean :: U.Vector Double -> Double
mean xs = s / fromIntegral n
  where
    Pair n s       = U.foldl' k (Pair 0 0) xs
    k (Pair n s) x = Pair (n+1) (s+x)

main :: IO ()
main = print (mean $ U.enumFromN 1 (10^7))
