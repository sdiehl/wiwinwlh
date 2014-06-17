{-# LANGUAGE TemplateHaskell #-}

import Splice

spliceF
spliceG "argument"

main = do
  print $ f 1 2
  print $ g ()
