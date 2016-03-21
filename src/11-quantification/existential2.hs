{-# LANGUAGE RankNTypes #-}

type Exists a b = forall f. Functor f => (b -> f b) -> (a -> f a)

type Get a b = a -> b
type Set a b = a -> b -> a

example :: Get a b -> Set a b -> Exists a b
example f g l a = fmap (g a) (l (f a))
