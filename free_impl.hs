{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Applicative

data Free f a
  = Pure a
  | Free (f (Free f a))

instance Functor f => Monad (Free f) where
  return a     = Pure a
  Pure a >>= f = f a
  Free f >>= g = Free (fmap (>>= g) f)

class Monad m => MonadFree f m  where
  wrap :: f (m a) -> m a

liftF :: (Functor f, MonadFree f m) => f a -> m a
liftF = wrap . fmap return

iter :: Functor f => (f a -> a) -> Free f a -> a
iter _ (Pure a) = a
iter phi (Free m) = phi (iter phi <$> m)

retract :: Monad f => Free f a -> f a
retract (Pure a) = return a
retract (Free as) = as >>= retract
