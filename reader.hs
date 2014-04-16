import Control.Monad.Reader

data MyState = MyState
  { foo :: String
  , bar :: Int
  } deriving (Show)

computation :: Reader MyState (Maybe String)
computation = do
  n <- asks bar
  x <- asks foo
  if n > 0
    then return (Just x)
    else return Nothing

example1 :: Maybe String
example1 = runReader computation $ MyState "hello!" 1

example2 :: Maybe String
example2 = runReader computation $ MyState "example!" 0
