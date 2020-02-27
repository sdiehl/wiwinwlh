{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)

app :: Application
app req respond = respond $ responseLBS status200 [] "Make it so."

main :: IO ()
main = run 8000 app
