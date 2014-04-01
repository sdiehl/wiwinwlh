{-# LANGUAGE DeriveDataTypeable #-}

import Data.Data

data Animal = Cat | Dog deriving (Data, Typeable)

example1 :: Constr
example1 = toConstr Dog
-- Dog

example2 :: DataType
example2 = dataTypeOf Cat
-- DataType {tycon = "Main.Animal", datarep = AlgRep [Cat,Dog]}
