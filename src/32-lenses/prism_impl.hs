{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Functor
import Data.Monoid

import Control.Applicative
import Data.Traversable

newtype Getting c a = Getting { unGetting :: c }
newtype Setting a = Setting { unSetting :: a }

type LensLike f s t a b = (a -> f b) -> s -> f t

type Lens a a' b b' = forall f. Functor f => LensLike f a a' b b'
type Lens' a b = Lens a a b b

type Prism a a' b b' = forall f. Applicative f => (b -> f b') -> (a -> f a')
type Prism' a b = Prism a a b b

type Setter a a' b b' = LensLike Setting a a' b b'
type Setter' a b = Setter a a b b

type Getter a c = forall r d b. (c -> Getting r d) -> a -> Getting r b

type FoldLike r a a' b b' = LensLike (Getting r) a a' b b'

instance Functor (Getting c) where
  fmap _ (Getting c) = Getting c

instance Monoid c => Applicative (Getting c) where
  pure _ = Getting mempty
  Getting a <*> Getting b = Getting (a `mappend` b)

class Functor f => Phantom f where
  coerce :: f a -> f b

instance Phantom (Getting c) where
  coerce (Getting c) = Getting c

instance Functor Setting where
  fmap f (Setting a) = Setting (f a)

instance Applicative Setting where
  pure = Setting
  Setting f <*> Setting a = Setting (f a)


lens :: (a -> b) -> (a -> b' -> a') -> Lens a a' b b'
lens getter setter f a = fmap (setter a) (f (getter a))

(.~) :: Setter a a' b b' -> b' -> a -> a'
l .~ b = l %~ const b

view :: FoldLike b a a' b b' -> a -> b
view l = unGetting . l Getting

over :: Setter a a' b b' -> (b -> b') -> a -> a'
over l = (l %~)

set :: Setter a a' b b' -> b' -> a -> a'
set = (.~)

(%~) :: Setter a a' b b' -> (b -> b') -> a -> a'
l %~ f = unSetting . l (Setting . f)

compose :: Lens a a' b b' -> Lens b b' c c' -> Lens a a' c c'
compose l s = l . s

id' :: Lens' a a
id' = id

infixl 1 &
infixr 4 .~
infixr 4 %~
infixr 8 ^.

(^.) :: a -> FoldLike b a a' b b' -> b
(^.) = flip view

(&) :: a -> (a -> b) -> b
(&) = flip ($)

(+~), (-~), (*~) :: Num b => Setter' a b -> b -> a -> a
f +~ b = f %~ (+b)
f -~ b = f %~ (subtract b)
f *~ b = f %~ (*b)


infixr 8 ^?
infixr 8 ^..

views :: FoldLike r a a' b b' -> (b -> r) -> a -> r
views l f = unGetting . l (Getting . f)

(^?) :: a -> FoldLike (First b) a a' b b' -> Maybe b
x ^? l = firstOf l x

(^..) :: a -> FoldLike [b] a a' b b' -> [b]
x ^.. l = toListOf l x

toListOf :: FoldLike [b] a a' b b' -> a -> [b]
toListOf l = views l (:[])

firstOf :: FoldLike (First b) a a' b b' -> a -> Maybe b
firstOf l = getFirst . views l (First . Just)

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism rm rv f a =
  case rv a of
    Right x -> fmap rm (f x)
    Left x  -> pure x

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' rm rv f a =
  case rv a of
    Just x  -> fmap rm (f x)
    Nothing -> pure a

_just :: Prism (Maybe a) (Maybe b) a b
_just = prism Just $ maybe (Left Nothing) Right

_nothing :: Prism' (Maybe a) ()
_nothing = prism' (const Nothing) $ maybe (Just ()) (const Nothing)

_right :: Prism (Either c a) (Either c b) a b
_right = prism Right $ either (Left . Left) Right

_left :: Prism (Either a c) (Either b c) a b
_left = prism Left $ either Right (Left . Right)

to :: (s -> a) -> Getter s a
to p f = coerce . f . p



pair :: (Int, Char)
pair = (1, 'b')

_1 :: Lens (a, b) (a', b) a a'
_1 f (a, b) = (\x -> (x, b)) <$> f a

_2 :: Lens (a, b) (a, b') b b'
_2 f (a, b) = (\x -> (a, x)) <$> f b

both :: Prism (a, a) (b, b) a b
both f (a, b) = (,) <$> f a <*> f b

ex1 = pair ^. _1
ex2 = pair ^. _2
ex3 = pair & _1 .~ "a"
ex4 = pair & (_1  %~ (+1))
           . (_2  .~ 1)

ex5 = (1, 2) & both .~ 1
ex6 = Just 3 & _just +~ 1
ex7 = (Left 3) ^? _left
ex8 = over traverse (+1) [1..25]

data Value
  = I Int
  | D Double
  deriving Show

_I :: Prism' Value Int
_I = prism remit review
  where
    remit :: Int -> Value
    remit a = I a

    review :: Value -> Either Value Int
    review (I a) = Right a
    review a     = Left a

ex9 :: Maybe Int
ex9 = (I 42) ^? _I

ex10 :: Value
ex10 = over _I succ (I 2)

ex11 :: Value
ex11 = over _I succ (D 2.71)
