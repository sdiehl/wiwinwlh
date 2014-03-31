import Control.Monad.Reader

data MyState = MyState
  { foo :: String
  , bar :: Int
  } deriving (Show)

example :: Reader MyState (Maybe String)
example = do
  n <- asks bar
  x <- asks foo
  if n > 0 then
    return (Just x)
  else
    return Nothing

main :: IO ()
main = do
  print $ runReader example $ MyState "hello!" 1
  print $ runReader example $ MyState "example!" 0
