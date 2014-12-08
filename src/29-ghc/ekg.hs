{-# Language OverloadedStrings #-}

import Control.Monad
import System.Remote.Monitoring

main :: IO ()
main = do
  ekg <- forkServer "localhost" 8000
  putStrLn "Started server on http://localhost:8000"
  forever $ getLine >>= putStrLn
