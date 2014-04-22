{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable
import Control.Monad.Catch
import Control.Monad.Identity

data MyException = MyException
    deriving (Show, Typeable)

instance Exception MyException

example :: MonadCatch m => Int -> Int -> m Int
example x y | y == 0 = throwM MyException
            | otherwise = return $ x `div` y

pure :: MonadCatch m => m (Either MyException Int)
pure = do
  a <- try (example 1 2)
  b <- try (example 1 0)
  return (a >> b)
