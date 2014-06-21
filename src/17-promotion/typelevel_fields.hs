{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}


import GHC.TypeLits

newtype Field (n :: Symbol) v = Field { unField :: v }
  deriving Show

data Person1 = Person1
  { _age      :: Field "age" Int
  , _name     :: Field "name" String
  }

data Person2 = Person2
  { _age'  :: Field "age" Int
  , _name' :: Field "name" String
  , _lib'  :: Field "lib" String
  }

deriving instance Show Person1
deriving instance Show Person2

data Label (l :: Symbol) = Get

class Has a l b | a l -> b where
  from :: a -> Label l -> b

instance Has Person1 "age" Int where
  from (Person1 a _) _ = unField a

instance Has Person1 "name" String where
  from (Person1 _ a) _ = unField a

instance Has Person2 "age" Int where
  from (Person2 a _ _) _ = unField a

instance Has Person2 "name" String where
  from (Person2 _ a _) _ = unField a

age :: Has a "age" b => a -> b
age pnt = from pnt (Get :: Label "age")

name :: Has a "name" b => a -> b
name pnt = from pnt (Get :: Label "name")

-- Parameterized constraint kind for "Simon-ness" of a record.
type Simon a = (Has a "name" String, Has a "age" Int)

spj :: Person1
spj = Person1 (Field 56) (Field "Simon Peyton Jones")

smarlow :: Person2
smarlow = Person2 (Field 38) (Field "Simon Marlow") (Field "rts")


catNames :: (Simon a, Simon b) => a -> b -> String
catNames a b = name a ++ name b

addAges :: (Simon a, Simon b) => a -> b -> Int
addAges a b = age a + age b


names :: String
names = name smarlow ++ "," ++ name spj
-- "Simon Marlow,Simon Peyton Jones"

ages :: Int
ages = age spj + age smarlow
-- 94
