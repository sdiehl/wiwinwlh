{-# LANGUAGE NoImplicitPrelude #-}

module Typeclasses where

import Prelude (not, Bool(..))

data Animal = Dog | Cat

class EqClass t where
  equal :: t -> t -> Bool
  neq   :: t -> t -> Bool

  neq a b = not (equal a b)

instance EqClass Animal where
  equal Dog Dog = True
  equal Cat Cat = True
  equal _ _ = False


data EqDict t = EqDict { equal' :: t -> t -> Bool }

equalAnimal Dog Dog = True
equalAnimal Cat Cat = True
equalAnimal _ _ = False

animalEq :: EqDict Animal
animalEq = EqDict equalAnimal

neqAnimal :: EqClass t => t -> t -> Bool
neqAnimal a b = neq a b

neqAnimal' :: EqDict t -> t -> t -> Bool
neqAnimal' dict a b = not (equal' dict a b)
