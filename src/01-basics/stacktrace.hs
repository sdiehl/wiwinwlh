import Control.Exception

f x = g x

g x = error (show x)

main = try (evaluate (f ())) :: IO (Either SomeException ())
