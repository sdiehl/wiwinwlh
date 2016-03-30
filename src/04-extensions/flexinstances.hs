{-# LANGUAGE FlexibleInstances #-}

class MyClass a

-- Without flexible instances, all instance heads must be type variable. The
-- following would be legal.
instance MyClass (Maybe a)

-- With flexible instances, typeclass heads can be arbitrary nested types. The
-- following would be forbidden without it.
instance MyClass (Maybe Int)
