{-# LANGUAGE FlexibleContexts #-}

class MyClass a

-- Without flexible contexts, all contexts must be type variable. The
-- following would be legal.
instance (MyClass a) => MyClass (Either a b)

-- With flexible contexts, typeclass contexts can be arbitrary nested types. The
-- following would be forbidden without it.
instance (MyClass (Maybe a)) => MyClass (Either a b)
