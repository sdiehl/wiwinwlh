example1 :: Maybe Int
example1 = do
  a <- Just 3
  b <- Just 4
  return $ a + b
-- Just 7

example2 :: Maybe Int
example2 = do
  a <- Just 3
  b <- Nothing
  return $ a + b
-- Nothing
