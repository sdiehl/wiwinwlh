{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Pandoc
import Text.Pandoc.Error

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
    Just f -> return . CodeBlock (id, classes, namevals) =<< T.readFile (T.unpack f)
    Nothing -> return cb
doInclude x = return x

doHtml :: Block -> IO Block
doHtml cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "literal" namevals of
    Just f -> return . RawBlock "html" =<< T.readFile (T.unpack f)
    Nothing -> return cb
doHtml x = return x

ropts :: ReaderOptions
ropts = def {readerExtensions = pandocExtensions}

wopts :: WriterOptions
wopts = def {writerExtensions = pandocExtensions}

main :: IO ()
main =
  runIOorExplode $
    liftIO T.getContents
      >>= readMarkdown ropts
      >>= liftIO . bottomUpM doInclude
      >>= liftIO . bottomUpM doHtml
      >>= writeMarkdown wopts
      >>= liftIO . T.putStrLn
