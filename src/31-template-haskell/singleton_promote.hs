{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.Singletons
import Data.Singletons.TH

$(promote [d|
  map :: (a -> b) -> [a] -> [b]
  map _ [] = []
  map f (x:xs) = f x : map f xs
  |])

infixr 5 :::

data HList (ts :: [ * ]) where
  Nil :: HList '[]
  (:::) :: t -> HList ts -> HList (t ': ts)

-- TypeLevel
-- MapJust :: [*] -> [Maybe *]
type MapJust xs = Map Maybe xs

-- Value Level
-- mapJust :: [a] -> [Maybe a]
mapJust :: HList xs -> HList (MapJust xs)
mapJust Nil = Nil
mapJust (x ::: xs) = (Just x) ::: mapJust xs

type A = [Bool, String , Double , ()]

a :: HList A
a = True ::: "foo" ::: 3.14 ::: () ::: Nil


example1 :: HList (MapJust A)
example1 = mapJust a

-- example1 reduces to example2 when expanded
example2 :: HList ([Maybe Bool, Maybe String , Maybe Double , Maybe ()])
example2 = Just True ::: Just "foo" ::: Just 3.14 ::: Just () ::: Nil
