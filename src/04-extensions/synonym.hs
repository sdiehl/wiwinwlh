{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

type IntList = [Int]

class MyClass a

-- Without type synonym instances, we're forced to manually expand out type
-- synonyms in the typeclass head.
instance MyClass [Int]

-- With it GHC will do this for us automatically. Type synonyms still need to
-- be fully applied.
instance MyClass IntList
