import Data.Monoid

newtype Writer w a = Writer { runWriter :: (a, w) }

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

instance Monoid w => Monad (Writer w) where
  return a = Writer (a, mempty)
  m >>= k  = Writer $ let
      (a, w)  = runWriter m
      (b, w') = runWriter (k a)
      in (b, w `mappend` w')


tell :: w -> Writer w ()
tell w = Writer ((), w)

listen :: Writer w t -> Writer w (t, w)
listen m = Writer $ let (a, w) = runWriter m in ((a, w), w)

pass :: Writer t (a, t -> w) -> Writer w a
pass m = Writer $ let ((a, f), w) = runWriter m in (a, f w)

main = return ()
