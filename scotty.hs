{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (toHtml, Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

greet :: String -> Html
greet user = H.html $ do
  H.head $
    H.title "Welcome!"
  H.body $ do
    H.h1 "Greetings!"
    H.p ("Hello " >> toHtml user >> "!")

app = do
  get "/" $
    text "Home Page"

  get "/greet/:name" $ do
    name <- param "name"
    html $ renderHtml (greet name)

main :: IO ()
main = scotty 8000 app
