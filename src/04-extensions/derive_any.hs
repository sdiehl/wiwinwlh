{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}

class MinimalClass a where
  const1 :: a -> Int
  default const1 :: a -> Int
  const1 _ = 1

  const2 :: a -> Int
  default const2 :: a -> Int
  const2 _ = 2

data Example = Example
  deriving (MinimalClass)

main :: IO ()
main = do
  print (const1 Example)
  print (const2 Example)
