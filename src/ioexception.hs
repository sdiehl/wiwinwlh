{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable
import Control.Exception

data MyException = MyException
    deriving (Show, Typeable)

instance Exception MyException

evil :: [Int]
evil = [throw MyException]

example1 :: Int
example1 = head evil

example2 :: Int
example2 = length evil

main :: IO ()
main = do
  a <- try (evaluate example1) :: IO (Either MyException Int)
  print a

  b <- try (return example2) :: IO (Either MyException Int)
  print b
