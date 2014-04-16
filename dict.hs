{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}

import GHC.Exts (Constraint)

data Dict :: Constraint -> * where
  Dict :: (c) => Dict c

dShow :: Dict (Show a) -> a -> String
dShow Dict x = show x

dEqNum :: Dict (Eq a, Num a) -> a -> Bool
dEqNum Dict x = x == 0


fShow :: String
fShow = dShow Dict 10

fEqual :: Bool
fEqual = dEqNum Dict 0
