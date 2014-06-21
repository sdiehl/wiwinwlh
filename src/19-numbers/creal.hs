import Data.Number.CReal

-- algebraic
phi :: CReal
phi = (1 + sqrt 5) / 2

-- transcendental
ramanujan :: CReal
ramanujan = exp (pi * sqrt 163)

main :: IO ()
main = do
  putStrLn $ showCReal 30 pi
  -- 3.141592653589793238462643383279
  putStrLn $ showCReal 30 phi
  -- 1.618033988749894848204586834366
  putStrLn $ showCReal 15 ramanujan
  -- 262537412640768743.99999999999925
