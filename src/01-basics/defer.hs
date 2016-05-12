{-# OPTIONS_GHC -fdefer-type-errors #-} -- Enable deferred type
                                        -- errors at module level

x :: ()
x = print 3

y :: Char
y = 0

z :: Int
z = 0 + "foo"

main :: IO ()
main = do
  print x
