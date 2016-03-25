{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Html where

import Text.Blaze.Html5
import Text.Blaze.Html.Renderer.Text

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

data Employee = Employee
  { name :: T.Text
  , age :: Int
  }

instance ToMarkup Employee where
  toMarkup Employee {..} = ul $ mconcat
    [ li (toHtml name)
    , li (toHtml age)
    ]

fred :: Employee
fred = Employee { name = "Fred", age = 35 }

main :: IO ()
main = do
  T.putStrLn $ renderHtml (toHtml fred)
