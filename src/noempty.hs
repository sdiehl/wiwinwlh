import Data.List.NonEmpty
import Prelude hiding (head, tail, foldl1)
import Data.Foldable (foldl1)

a :: NonEmpty Integer
a = fromList [1,2,3]
-- 1 :| [2,3]

b :: NonEmpty Integer
b = 1 :| [2,3]
-- 1 :| [2,3]

c :: NonEmpty Integer
c = fromList []
-- *** Exception: NonEmpty.fromList: empty list

d :: Integer
d = foldl1 (+) $ fromList [1..100]
-- 5050
