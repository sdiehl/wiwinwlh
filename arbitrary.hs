import Test.QuickCheck

data Color = Red | Green | Blue deriving Show

instance Arbitrary Color where
  arbitrary = do
    n <- choose (0,2) :: Gen Int
    return $ case n of
      0 -> Red
      1 -> Green
      2 -> Blue

example1 :: IO [Color]
example1 = sample' arbitrary
-- [Red,Green,Red,Blue,Red,Red,Red,Blue,Green,Red,Red]
