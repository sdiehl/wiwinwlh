import Control.Monad

id' :: (->) a a
id' = id

const' :: (->) a ((->) b a)
const' = const

-- Monad m => a -> m a
fret :: a -> b -> a
fret = return

-- Monad m => m a -> (a -> m b) -> m b
fbind :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
fbind f k = f >>= k

-- Monad m => m (m a) -> m a
fjoin :: (r -> (r -> a)) -> (r -> a)
fjoin = join

fid :: a -> a
fid = const >>= id

-- Functor f => (a -> b) -> f a -> f b
fcompose :: (a -> b) -> (r -> a) -> (r -> b)
fcompose = (.)
