{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data Rec = MkRec { _foo :: Int , _bar :: Int } deriving Show
makeLenses ''Rec

x :: Rec
x = MkRec { _foo = 1024, _bar = 1024 }

get1 :: Int
get1 = (_foo x) + (_bar x)

get2 :: Int
get2 = (x ^. foo) + (x ^. bar)

get3 :: Int
get3 = (view foo x) + (view bar x)


set1 :: Rec
set1 = x { _foo = 1, _bar = 2 }

set2 :: Rec
set2 = x & (foo .~ 1) . (bar .~ 2)

set3 :: Rec
set3 = x & (set foo 1) . (set bar 2)
