import Data.Dynamic
import Data.Maybe

dynamicBox :: Dynamic
dynamicBox = toDyn (6.62 :: Double)

example1 :: Maybe Int
example1 = fromDynamic dynamicBox
-- Nothing

example2 :: Maybe Double
example2 = fromDynamic dynamicBox
-- Just 6.62

example3 :: Int
example3 = fromDyn dynamicBox 0
-- 0

example4 :: Double
example4 = fromDyn dynamicBox 0.0
-- 6.62
