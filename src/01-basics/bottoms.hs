import GHC.Err
import Prelude hiding (head, (!!), undefined)

-- Annotated code that features use of the error function.

error :: String -> a                       -- Takes an error message of type
                                           -- String and returns what ever type
                                           -- is needed

divByY:: (Num a, Eq a, Fractional a) => a -> a -> a
divByY _ 0 = error "Divide by zero error"      -- Dividing by 0 causes an error
divByY dividend divisor = dividend / divisor   -- Handles defined division

-- degenerate functions

undefined :: a
undefined =  error "Prelude.undefined"

head :: [a] -> a
head (x:_) =  x
head []    =  error "Prelude.head: empty list"

(!!) :: [a] -> Int -> a
xs     !! n | n < 0 =  error "Prelude.!!: negative index"
[]     !! _         =  error "Prelude.!!: index too large"
(x:_)  !! 0         =  x
(_:xs) !! n         =  xs !! (n-1)
