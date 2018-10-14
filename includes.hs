{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc
import Text.Pandoc.Error
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
       Just f     -> return . CodeBlock (id, classes, namevals) =<< readFile f
       Nothing    -> return cb
doInclude x = return x

doHtml :: Block -> IO Block
doHtml cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "literal" namevals of
       Just f     -> return . RawBlock "html" =<< readFile f
       Nothing    -> return cb
doHtml x = return x

ropts = def { readerExtensions = pandocExtensions }
wopts = def { writerExtensions = pandocExtensions }

main :: IO ()
main = runIOorExplode $ liftIO T.getContents
                    >>= readMarkdown ropts
                    >>= liftIO . bottomUpM doInclude
                    >>= liftIO . bottomUpM doHtml
                    >>= writeMarkdown wopts
                    >>= liftIO . T.putStrLn

-- TESTING THE PANDOC MONAD
-- pandocMonadTest0 = runIOorExplode $ lookupEnv @PandocIO "NIX_GHC" >>= liftIO . print
