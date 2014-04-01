import Data.STRef
import Control.Monad
import Control.Monad.ST

example :: Integer
example = runST $ do
  x <- newSTRef 0

  forM_ [1..1000] $ \j -> do
    writeSTRef x j

  readSTRef x
