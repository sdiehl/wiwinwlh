import Data.Text
import Text.Printf

a :: Int
a = 3

b :: Double
b = 3.14159

c :: String
c = "haskell"

example :: String
example = printf "(%i, %f, %s)" a b c
-- "(3, 3.14159, haskell)"
