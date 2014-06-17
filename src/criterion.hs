import Criterion.Main
import Criterion.Config

-- Naive recursion for fibonacci numbers.
fib1 :: Int -> Int
fib1 0 = 0
fib1 1 = 1
fib1 n = fib1 (n-1) + fib1 (n-2)

-- Use the De Moivre closed form for fibonacci numbers.
fib2 :: Int -> Int
fib2 x = truncate $ ( 1 / sqrt 5 ) * ( phi ^ x - psi ^ x )
  where
      phi = ( 1 + sqrt 5 ) / 2
      psi = ( 1 - sqrt 5 ) / 2

suite :: [Benchmark]
suite = [
    bgroup "naive" [
      bench "fib 10" $ whnf fib1 5
    , bench "fib 20" $ whnf fib1 10
    ],
    bgroup "de moivre" [
      bench "fib 10" $ whnf fib2 5
    , bench "fib 20" $ whnf fib2 10
    ]
  ]

main :: IO ()
main = defaultMain suite
