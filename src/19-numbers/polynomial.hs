import Data.Poly

abel :: VPoly Integer
abel = X ^ 5 - X + 1

fibPoly :: Integer -> VPoly Integer
fibPoly 0 = 0
fibPoly 1 = 1
fibPoly n = X * fibPoly (n - 1) + fibPoly (n - 2)

division :: (VPoly Double, VPoly Double)
division = gcdExt (X ^ 3 - 2 * X ^ 2 - 4) (X - 3)
