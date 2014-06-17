{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}


import GHC.TypeLits
import Data.Type.Equality

data Label (l :: Symbol) = Get

class Has a l b | a l -> b where
  from :: a -> Label l -> b

data Point2D = Point2 Double Double deriving Show
data Point3D = Point3 Double Double Double deriving Show

instance Has Point2D "x" Double where
  from (Point2 x _) _ = x

instance Has Point2D "y" Double where
  from (Point2 _ y) _ = y


instance Has Point3D "x" Double where
  from (Point3 x _ _) _ = x

instance Has Point3D "y" Double where
  from (Point3 _ y _) _ = y

instance Has Point3D "z" Double where
  from (Point3 _ _ z) _ = z


infixl 6 #

(#) :: a -> (a -> b) -> b
(#) = flip ($)

_x :: Has a "x" b => a -> b
_x pnt = from pnt (Get :: Label "x")

_y :: Has a "y" b => a -> b
_y pnt = from pnt (Get :: Label "y")

_z :: Has a "z" b => a -> b
_z pnt = from pnt (Get :: Label "z")

type Point a r = (Has a "x" r, Has a "y" r)

distance :: (Point a r, Point b r, Floating r) => a -> b -> r
distance p1 p2 = sqrt (d1^2 + d2^2)
  where
    d1 = (p1 # _x) + (p1 # _y)
    d2 = (p2 # _x) + (p2 # _y)

main :: IO ()
main = do
  print $ (Point2 10 20) # _x

  -- Fails with: No instance for (Has Point2D "z" a0)
  -- print $ (Point2 10 20) # _z

  print $ (Point3 10 20 30) # _x
  print $ (Point3 10 20 30) # _z

  print $ distance (Point2 1 3) (Point2 2 7)
  print $ distance (Point2 1 3) (Point3 2 7 4)
  print $ distance (Point3 1 3 5) (Point3 2 7 3)
