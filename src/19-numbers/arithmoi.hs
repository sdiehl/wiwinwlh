import Data.Maybe
import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.Moduli.Sqrt
import Math.NumberTheory.Primes
import Math.NumberTheory.Zeta

-- Riemann zeta function
exampleZeta :: Double
exampleZeta = zetas 1e-10 !! 10

-- Euler totient function
exampleEuler :: Integer
exampleEuler = totient 25

-- Ramanujan tau function
exampleRamanujan :: Integer
exampleRamanujan = ramanujan 16

-- Primality testing
examplePrimality :: Maybe (Prime Integer)
examplePrimality = isPrime 2147483647

-- Square roots moduluo prime
exampleSqrt :: [Integer]
exampleSqrt = sqrtsModPrime 42 (fromJust examplePrimality)
