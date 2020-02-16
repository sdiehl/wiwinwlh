{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lucid
import Lucid.Base
import Lucid.Html5

example1 :: Html ()
example1 = table_ (tr_ (td_ (p_ "My table.")))

example2 :: Html ()
example2 = html_ do
  head_ do
    title_ "HTML from Haskell"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "bootstrap.css"]
  body_ do
    p_ "Generating HTMl form Haskell datatypes:"
    ul_ $ mapM_ (li_ . toHtml . show) [1 .. 100]

main :: IO ()
main = do
  print (renderText example1)
  print (renderBS example2)
