{-# LANGUAGE OverloadedStrings #-}

module Html where

import Text.Blaze.Html5
import Text.Blaze.Html.Renderer.Text

import qualified Data.Text.Lazy.IO as T

example :: Html
example = do
  h1 "First header"
  p $ ul $ mconcat [li "First", li "Second"]

main :: IO ()
main = do
  T.putStrLn $ renderHtml example
