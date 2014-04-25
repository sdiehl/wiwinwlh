{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Lens
import Numeric.Lens
import Data.Complex.Lens

import Data.Complex
import qualified Data.Map as Map

l :: Num t => t
l = view _1 (100, 200)
-- 100

m :: (Num t) => (t, t, t)
m = (100,200,200) & _3 %~ (+100)
-- (100,200,300)

n :: Num a => [a]
n = [100,200,300] & traverse %~ (+1)
-- [101,201,301]

o :: Char
o = "frodo" ^?! ix 3
-- 'd'

p :: Num a => [a]
p = [[1,2,3], [4,5,6]] ^. traverse
-- [1,2,3,4,5,6]

q :: Num a => [a]
q = [1,2,3,4,5] ^. _tail
-- [2,3,4,5]

r :: Num a => [Maybe a]
r = [Just 1, Just 2, Just 3] & traverse._Just +~ 1
-- [Just 2, Just 3, Just 4]

s :: Maybe String
s = Map.fromList [("foo", "bar")] ^.at "foo"
-- "bar"

t :: Integral a => Maybe a
t = "1010110" ^? binary
-- Just 86

u :: RealFloat a => Complex a
u = (mkPolar 1 pi/2) & _phase +~ pi

v :: IO [String]
v = ["first","second","third"] ^!! folded.act ((>> getLine) . putStrLn)
-- first
-- a
-- second
-- b
-- third
-- c
-- ["a","b","c"]
