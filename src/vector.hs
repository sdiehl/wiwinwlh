import Data.Vector.Unboxed as V

norm ::  Vector Double -> Double
norm = sqrt . V.sum . V.map (\x -> x*x)

example1 :: Double
example1 = norm $ V.iterateN 100000000 (+1) 0.0
