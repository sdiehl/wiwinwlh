{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types

app :: Application
app req = return $ responseLBS status200 [] "Engage!"

main :: IO ()
main = run 8000 app
