{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import GHC.Generics

data Animal
  = Dog
  | Cat

instance Generic Animal where
  type
    Rep Animal =
      D1 ( 'MetaData "Animal" "Main" "main" 'False )
         ( C1 ( 'MetaCons "Dog" 'PrefixI 'False)
           U1 :+: C1 ( 'MetaCons "Cat" 'PrefixI 'False) U1
         )

  from Dog = M1 (L1 (M1 U1))
  from Cat = M1 (R1 (M1 U1))

  to (M1 (L1 (M1 U1))) = Dog
  to (M1 (R1 (M1 U1))) = Cat

data T_Animal -- Animal type
data C_Dog -- Dog Constructor
data C_Cat -- Cat Constructor

instance Datatype T_Animal where
  datatypeName _ = "Animal"
  moduleName _ = "Main"
  packageName _ = "main"

instance Constructor C_Dog where
  conName _ = "Dog"

instance Constructor C_Cat where
  conName _ = "Cat"
