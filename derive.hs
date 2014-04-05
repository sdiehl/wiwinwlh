{-# LANGUAGE TemplateHaskell #-}

import Data.DeriveTH
import Test.QuickCheck

data Color = Red | Green | Blue deriving Show

$(derive makeArbitrary ''Color)

example1 :: IO [Color]
example1 = sample' arbitrary
-- [Red,Green,Blue,Red,Blue,Green,Blue,Red,Blue,Blue,Red]
