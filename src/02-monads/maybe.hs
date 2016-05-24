example1 :: Maybe Int
example1 = do
  a <- Just 3                -- Bind 3 to name a
  b <- Just 4                -- Bind 4 to name b
  return $ a + b             -- Evaluate (a + b), then use 'return' to ensure
                             -- the result is in the Maybe monad in order to
                             -- satisfy the type signature
-- Just 7

example1Desugared :: Maybe Int
example1Desugared = Just 3 >>= \a ->    -- This example is the desugared
                      Just 4 >>= \b ->  -- equivalent to example1
                        return $ a + b

example2 :: Maybe Int
example2 = do
  a <- Just 3
  b <- Nothing
  return $ a + b
-- Nothing
