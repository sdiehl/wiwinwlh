{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Singleton

zero :: [snat|0|]
zero =  [snat|0|]

one :: [snat|1|]
one =  [snat|1|]

two :: [snat|2|]
two =  [snat|2|]

three :: [snat|3|]
three  = [snat|3|]

test :: SNat a -> Int
test x = case x of
  [snat|0|] -> 0
  [snat|1|] -> 1
  [snat|2|] -> 2
  [snat|3|] -> 3

isZero :: SNat a -> Bool
isZero [snat|0|] = True
isZero _ = False
