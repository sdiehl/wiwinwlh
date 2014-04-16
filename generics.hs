{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

import GHC.Generics

data Animal
  = Dog
  | Cat

instance Generic Animal where
  type Rep Animal = D1 T_Animal ((C1 C_Dog U1) :+: (C1 C_Cat U1))

  from Dog = M1 (L1 (M1 U1))
  from Cat = M1 (R1 (M1 U1))

  to (M1 (L1 (M1 U1))) = Dog
  to (M1 (R1 (M1 U1))) = Cat

data T_Animal
data C_Dog
data C_Cat

instance Datatype T_Animal where
  datatypeName _ = "Animal"
  moduleName _ = "Main"

instance Constructor C_Dog where
  conName _ = "Dog"

instance Constructor C_Cat where
  conName _ = "Cat"
