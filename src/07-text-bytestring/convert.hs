{-# LANGUAGE OverloadedStrings #-}

import Data.String.Conv

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Monoid

a :: String
a = "Gödel"

b :: BL.ByteString
b = "Einstein"

c :: T.Text
c = "Feynmann"

d :: B.ByteString
d = "Schrödinger"

-- Compare unlike strings
(==~) :: (Eq a, StringConv b a) => a -> b -> Bool
(==~) a b = a == toS b

-- Concat unlike strings
(<>~) :: (Monoid a, StringConv b a) => a -> b -> a
(<>~) a b = a <> toS b

main :: IO ()
main = do
  putStrLn (toS a)
  TL.putStrLn (toS b)
  print (a ==~ b)
  print (c ==~ d)
  print (c ==~ c)
  print (b <>~ c)
