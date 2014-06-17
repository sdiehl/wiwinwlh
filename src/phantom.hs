import Data.Void

data Foo tag a = Foo a

combine :: Num a => Foo tag a -> Foo tag a -> Foo tag a
combine (Foo a) (Foo b) = Foo (a+b)

-- All identical at the value level, but differ at the type level.
a :: Foo () Int
a = Foo 1

b :: Foo t Int
b = Foo 1

c :: Foo Void Int
c = Foo 1

-- () ~ ()
example1 :: Foo () Int
example1 = combine a a

-- t ~ ()
example2 :: Foo () Int
example2 = combine a b

-- t0 ~ t1
example3 :: Foo t Int
example3 = combine b b

-- Couldn't match type `t' with `Void'
example4 :: Foo t Int
example4 = combine b c
