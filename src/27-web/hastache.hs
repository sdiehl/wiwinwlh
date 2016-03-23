{-# LANGUAGE OverloadedStrings #-}

import Text.Hastache
import Text.Hastache.Context

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Data.Data

template :: FilePath -> MuContext IO -> IO TL.Text
template = hastacheFile defaultConfig

-- Function strContext
context :: String -> MuType IO
context "body"  = MuVariable ("Hello World" :: TL.Text)
context "title" = MuVariable ("Haskell is lovely" :: TL.Text)
context _       = MuVariable ()

main :: IO ()
main = do
  output <- template "templates/home.html" (mkStrContext context)
  TL.putStrLn output
