{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mconcat, (<>))

import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import qualified Data.Text.Lazy.IO as L

beer :: Int -> Builder
beer n = decimal n <> " bottles of beer on the wall.\n"

wall :: Builder
wall = mconcat $ fmap beer [1..1000]

main :: IO ()
main = L.putStrLn $ toLazyText wall
