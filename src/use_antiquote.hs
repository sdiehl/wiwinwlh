{-# LANGUAGE QuasiQuotes #-}

import Antiquote

-- extract
a :: Expr -> Expr
a [mini|succ $x|] = x

b :: Expr -> Expr
b [mini|succ $x|] = [mini|pred $x|]

c :: Expressible a => a -> Expr
c x = [mini|succ $x|]

d :: Expr
d = c (8 :: Integer)
-- Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))

e :: Expr
e = c True
-- Succ Tr
