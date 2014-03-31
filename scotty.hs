{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 hiding (html, param)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Data.Default (def)
import Network.Wai.Handler.Warp (settingsPort)

config :: Options
config = def { settings = (settings def) { settingsPort = 4000 } }

greet :: String -> Html
greet user = H.html $ do
  H.head $
    H.title "Welcome!"
  H.body $ do
    H.h1 "Greetings!"
    H.p ("Hello " >> toHtml user >> "!")

main :: IO ()
main = do
    putStrLn "Starting HTTP Server."
    scottyOpts config $ do

      get "/greet/:name" $ do
          name <- param "name"
          html $ renderHtml (greet name)
