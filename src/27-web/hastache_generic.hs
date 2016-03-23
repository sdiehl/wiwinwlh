{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Text.Hastache
import Text.Hastache.Context

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Data.Data

template :: FilePath -> MuContext IO -> IO TL.Text
template = hastacheFile defaultConfig

-- Record context
data TemplateCtx = TemplateCtx
  { body :: TL.Text
  , title :: TL.Text
  } deriving (Data, Typeable)

main :: IO ()
main = do
  let ctx = TemplateCtx { body = "Hello", title = "Haskell" }
  output <- template "templates/home.html" (mkGenericContext ctx)
  TL.putStrLn output
