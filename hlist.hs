{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

infixr 5 :::

data HList (ts :: [ * ]) where
  Nil :: HList '[]
  (:::) :: t -> HList ts -> HList (t ': ts)

-- Take the head of a non-empty list with the first value as Bool type.
headBool :: HList (Bool ': xs) -> Bool
headBool hlist = case hlist of
  (a ::: _) -> a

hlength :: HList x -> Int
hlength Nil = 0
hlength (_ ::: b) = 1 + (hlength b)


example1 :: (Bool, (String, (Double, ())))
example1 = (True, ("foo", (3.14, ())))

example2 :: HList '[Bool, String , Double , ()]
example2 = True ::: "foo" ::: 3.14 ::: () ::: Nil
