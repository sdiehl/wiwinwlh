{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

data Free f a
  = Pure a
  | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f x = go x
    where
      go (Free fa) = Free (go <$> fa)

instance Applicative f => Applicative (Free f) where
  pure = Pure
  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> Pure b = Free $ fmap ($ b) <$> ma
  Free ma <*> Free mb = Free $ fmap (<*>) ma <*> mb

instance Applicative f => Monad (Free f) where
  return = Pure
  Pure a >>= f = f a
  Free f >>= g = Free (fmap (>>= g) f)

class Monad m => MonadFree f m where
  wrap :: f (m a) -> m a

instance Applicative f => MonadFree f (Free f) where
  wrap = Free

liftF :: (Functor f, MonadFree f m) => f a -> m a
liftF = wrap . fmap return

iter :: Functor f => (f a -> a) -> Free f a -> a
iter _ (Pure a) = a
iter phi (Free m) = phi (iter phi <$> m)

retract :: Monad f => Free f a -> f a
retract (Pure a) = return a
retract (Free as) = as >>= retract
