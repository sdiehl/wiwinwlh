import Prelude hiding (lookup)

import Control.Monad.ST
import Data.HashTable.ST.Basic

-- Hashtable parameterized by ST "thread"
type HT s = HashTable s String String

example1 :: ST s (HT s)
example1 = do
  ht <- new
  insert ht "key" "value1"
  return ht

example2 :: HT s -> ST s (Maybe String)
example2 ht = do
  val <- lookup ht "key"
  return val

example3 :: Maybe String
example3 = runST (example1 >>= example2)
