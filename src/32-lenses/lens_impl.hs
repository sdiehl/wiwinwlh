{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Functor

type Lens' a b = forall f. Functor f => (b -> f b) -> (a -> f a)

newtype Const x a  = Const { runConst :: x } deriving Functor
newtype Identity a = Identity { runIdentity :: a } deriving Functor

lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens getter setter f a = fmap (setter a) (f (getter a))

set :: Lens' a b -> b -> a -> a
set l b = runIdentity . l (const (Identity b))

view :: Lens' a b -> a -> b
view l = runConst . l Const

over :: Lens' a b -> (b -> b) -> a -> a
over l f a = set l (f (view l a)) a

compose :: Lens' a b -> Lens' b c -> Lens' a c
compose l s = l . s

id' :: Lens' a a
id' = id

infixl 1 &
infixr 4 .~
infixr 4 %~
infixr 8 ^.

(^.) = flip view
(.~) = set
(%~) = over

(&) :: a -> (a -> b) -> b
(&) = flip ($)

(+~), (-~), (*~) :: Num b => Lens' a b -> b -> a -> a
f +~ b = f %~ (+b)
f -~ b = f %~ (subtract b)
f *~ b = f %~ (*b)

-- Usage

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

foo :: Foo
foo = Foo 3

bar :: Bar
bar = Bar foo

example1 = view a foo
example2 = set a 1 foo
example3 = over a (+1) foo
example4 = view (b `compose` a) bar

example1' = foo  ^. a
example2' = foo  &  a .~ 1
example3' = foo  &  a %~ (+1)
example4' = bar  ^. b . a
