
-- Annotated code that features use of the error function.

divByY:: (Num a, Eq a, Fractional a) => a -> a -> a
divByY _ 0 = error "Divide by zero error"      -- Dividing by 0 causes an error
divByY dividend divisor = dividend / divisor   -- Handles defined division


