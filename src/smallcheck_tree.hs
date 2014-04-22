{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Test.SmallCheck.Series

data Tree a = Null | Fork (Tree a) a (Tree a)
    deriving (Show, Generic)

instance Serial m a => Serial m (Tree a)

example :: [Tree ()]
example = list 3 series

main = print example
