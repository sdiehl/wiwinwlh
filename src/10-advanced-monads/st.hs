import Data.STRef
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict

example1 :: Int
example1 = runST $ do
  x <- newSTRef 0

  forM_ [1..1000] $ \j -> do
    writeSTRef x j

  readSTRef x

example2 :: Int
example2 = runST $ do
  count <- newSTRef 0
  replicateM_ (10^6) $ modifySTRef' count (+1)
  readSTRef count

example3 :: Int
example3 = flip evalState 0 $ do
  replicateM_ (10^6) $ modify' (+1)
  get

modify' :: MonadState a m => (a -> a) -> m ()
modify' f = get >>= (\x -> put $! f x)
