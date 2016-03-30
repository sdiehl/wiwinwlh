{-# LANGUAGE DeriveDataTypeable #-}

import Data.Data
import Data.Typeable
import Data.Generics.Schemes
import Data.Generics.Aliases (mkT)

data MyTuple a = MyTuple a Float
  deriving (Data, Typeable, Show)

exampleT :: Data a => MyTuple a -> MyTuple a
exampleT = everywhere (mkT go1) . everywhere (mkT go2)
  where
    go1 :: Int -> Int
    go1 x = succ x

    go2 :: Float -> Float
    go2 x = succ x

findFloat :: Data x => x -> Maybe Float
findFloat = gfindtype

main :: IO ()
main = do
  let term = MyTuple (MyTuple (1 :: Int) 2.0) 3.0
  print (exampleT term)
  print (gsize term)
  print (findFloat term)
  print (listify ((>0) :: (Int -> Bool)) term)
