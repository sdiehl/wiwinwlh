{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Functor

type Lens a a' b b' = forall f. Functor f => (b -> f b') -> (a -> f a')
type Lens' a b = Lens a a b b

newtype Const x a  = Const { runConst :: x } deriving Functor
newtype Identity a = Identity { runIdentity :: a } deriving Functor

lens :: (a -> b) -> (a -> b' -> a') -> Lens a a' b b'
lens getter setter f a = fmap (setter a) (f (getter a))

set :: Lens a a' b b' -> b' -> a -> a'
set l b = runIdentity . l (const (Identity b))

get :: Lens a a' b b' -> a -> b
get l = runConst . l Const

over :: Lens a a' b b' -> (b -> b') -> a -> a'
over l f a = set l (f (get l a)) a

compose :: Lens a a' b b' -> Lens b b' c c' -> Lens a a' c c'
compose l s = l . s

id' :: Lens a a a a
id' = id

infixl 1 &
infixr 4 .~
infixr 4 %~
infixr 8 ^.

(^.) = flip get
(.~) = set
(%~) = over

(&) :: a -> (a -> b) -> b
(&) = flip ($)

(+~), (-~), (*~) :: Num b => Lens a a b b -> b -> a -> a
f +~ b = f %~ (+b)
f -~ b = f %~ (subtract b)
f *~ b = f %~ (*b)

-- Monomorphic Update
data Foo = Foo { _a :: Int } deriving Show
data Bar = Bar { _b :: Foo } deriving Show

a :: Lens' Foo Int
a = lens getter setter
  where
    getter :: Foo -> Int
    getter = _a

    setter :: Foo -> Int -> Foo
    setter = (\f new -> f { _a = new })

b :: Lens' Bar Foo
b = lens getter setter
  where
    getter :: Bar -> Foo
    getter = _b

    setter :: Bar -> Foo -> Bar
    setter = (\f new -> f { _b = new })

-- Polymorphic Update
data Pair a b = Pair a b deriving Show

pair :: Pair Int Char
pair = Pair 1 'b'

_1 :: Lens (Pair a b) (Pair a' b) a a'
_1 f (Pair a b) = (\x -> Pair x b) <$> f a

_2 :: Lens (Pair a b) (Pair a b') b b'
_2 f (Pair a b) = (\x -> Pair a x) <$> f b

ex1 = pair ^. _1
ex2 = pair ^. _2
ex3 = pair & _1 .~ "a"
ex4 = pair & (_1  %~ (+1))
           . (_2  .~ 1)
