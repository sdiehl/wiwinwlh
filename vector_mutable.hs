import GHC.Prim
import Control.Monad
import Data.Vector.Unboxed (freeze)
import qualified Data.Vector.Unboxed.Mutable as M

example :: IO (M.MVector RealWorld Int)
example = do
  v <- M.new 10
  forM_ [0..9] $ \i ->
     M.write v i i
  return v

main :: IO ()
main = example >>= freeze >>= print
