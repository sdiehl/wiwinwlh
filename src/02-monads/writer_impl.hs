import Data.Monoid

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Monoid w => Monad (Writer w) where
  return a = Writer (a, mempty)
  m >>= k  = Writer $ let
      (a, w)  = runWriter m
      (b, w') = runWriter (k a)
      in (b, w `mappend` w')

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

tell :: w -> Writer w ()
tell w = Writer ((), w)
