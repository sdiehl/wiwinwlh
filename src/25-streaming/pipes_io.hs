{-# LANGUAGE MultiWayIf #-}

import Pipes
import qualified Pipes.Prelude as P

count :: Producer Integer IO ()
count = each [1..100]

fizzbuzz :: Pipe Integer String IO ()
fizzbuzz = do
  n <- await
  if | n `mod` 15 == 0 -> yield "FizzBuzz"
     | n `mod` 5  == 0 -> yield "Fizz"
     | n `mod` 3  == 0 -> yield "Buzz"
     | otherwise       -> return ()
  fizzbuzz

main :: IO ()
main = runEffect $ count >-> fizzbuzz >-> P.stdoutLn
