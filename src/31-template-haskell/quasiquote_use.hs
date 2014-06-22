{-# LANGUAGE QuasiQuotes #-}

import Quasiquote

a :: Expr
a = [calc|true|]
-- Tr

b :: Expr
b = [calc|succ (succ 0)|]
-- Succ (Succ Zero)

c :: Expr
c = [calc|pred (succ 0)|]
-- Pred (Succ Zero)
