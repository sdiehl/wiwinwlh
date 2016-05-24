example1 :: Maybe Int
example1 = do
  a <- Just 3                -- Bind 3 to name a
  b <- Just 4                -- Bind 4 to name b
  return $ a + b             -- Evaluate (a + b), then use 'return' to ensure
                             -- the result is in the Maybe monad in order to
                             -- satisfy the type signature
-- Just 7

example2 :: Maybe Int
example2 = do
  a <- Just 3
  b <- Nothing
  return $ a + b
-- Nothing
