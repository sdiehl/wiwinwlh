example1 :: Maybe Int
example1 = do
  a <- Just 3                -- Bind 3 to name a
  b <- Just 4                -- Bind 4 to name b
  return $ a + b             -- Evaluate (a + b), then use 'return' to ensure
                             -- the result is in the Maybe monad in order to
                             -- satisfy the type signature
-- Just 7

desugared1 :: Maybe Int
desugared1 = Just 3 >>= \a ->    -- This example is the desugared
               Just 4 >>= \b ->  -- equivalent to example1
                 return $ a + b
-- Just 7

example2 :: Maybe Int
example2 = do
  a <- Just 3                -- Bind 3 to name a
  b <- Nothing               -- Bind Nothing to name b
  return $ a + b
-- Nothing                   -- This result might be somewhat surprising, since
                             -- addition within the Maybe monad can actually
                             -- return 'Nothing'. This result occurs because one
                             -- of the values, Nothing, indicates computational
                             -- failure. Since the computation failed at one
                             -- step within the process, the whole computation
                             -- fails, leaving us with 'Nothing' as the final
                             -- result.

desugared2 :: Maybe Int
desugared2 = Just 3 >>= \a ->     -- This example is the desugared
               Nothing >>= \b ->  -- equivalent to example2
                 return $ a + b
-- Nothing
