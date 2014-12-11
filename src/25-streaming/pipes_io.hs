{-# LANGUAGE MultiWayIf #-}

import Pipes
import qualified Pipes.Prelude as P

import Control.Monad

a :: Producer Integer IO ()
a = each [1..100]

b :: Pipe Integer String IO ()
b = do
  n <- await
  if | n `mod` 15 == 0 -> yield "FizzBuzz"
     | n `mod` 5  == 0 -> yield "Fizz"
     | n `mod` 3  == 0 -> yield "Buzz"
     | otherwise       -> return ()
  b

main = runEffect $ a >-> b >-> P.stdoutLn
