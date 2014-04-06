import GHC.Prim
import Control.Monad
import Data.Vector.Unboxed (freeze)
import Data.Vector.Unboxed.Mutable

example :: IO (MVector RealWorld Int)
example = do
  v <- new 10
  forM [0..9] $ \i ->
     write v i i
  return v

main :: IO ()
main = example >>= freeze >>= print
