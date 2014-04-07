import Pipes
import Pipes.Prelude
import Control.Monad

a = forM_ [1..10] yield

b =  forever $ do
  x <- await
  yield (x*2)
  yield (x*3)
  yield (x*4)

c = forever $ do
  x <- await
  if (x `mod` 2) == 0
    then yield x
    else return ()

contrived = toList $ a >-> b >-> c
