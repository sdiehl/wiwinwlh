{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Lens
import Numeric.Lens
import Data.Complex.Lens

import Data.Complex
import qualified Data.Map as Map

l :: Num t => t
l = view _1 (100, 200)
-- [100,200,300]

m :: (Num t) => (t, t, t)
m = (100,200,200) & _3 %~ (+100)
-- [100,200,300]

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

r :: Maybe String
r = Map.fromList [("foo", "bar")] ^.at "foo"
-- "bar"

s :: Integral a => Maybe a
s = "1010110" ^? binary
-- Just 86

t :: RealFloat a => Complex a
t = (mkPolar 1 pi/2) & _phase +~ pi

u :: IO [String]
u = ["first","second","third"] ^!! folded.act ((>> getLine) . putStrLn)
-- first
-- a
-- second
-- b
-- third
-- c
-- ["a","b","c"]
