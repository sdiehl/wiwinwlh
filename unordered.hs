import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M

example1 :: M.HashMap Int Char
example1 = M.fromList $ zip [1..10] ['a'..]

example2 :: S.HashSet Int
example2 = S.fromList [1..10]
