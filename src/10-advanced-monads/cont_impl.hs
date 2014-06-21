newtype Cont r a = Cont { runCont :: ((a -> r) -> r) }

instance Monad (Cont r) where
  return a       = Cont $ \k -> k a
  (Cont c) >>= f = Cont $ \k -> c (\a -> runCont (f a) k)

class (Monad m) => MonadCont m where
  callCC :: ((a -> m b) -> m a) -> m a

instance MonadCont (Cont r) where
  callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k
