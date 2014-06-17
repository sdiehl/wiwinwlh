{-# LANGUAGE OverloadedStrings #-}

-- includes.hs
import Text.Pandoc

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
       Just f     -> return . (CodeBlock (id, classes, namevals)) =<< readFile f
       Nothing    -> return cb
doInclude x = return x

doHtml :: Block -> IO Block
doHtml cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "literal" namevals of
       Just f     -> return . (RawBlock "html") =<< readFile f
       Nothing    -> return cb
doHtml x = return x

main :: IO ()
main = getContents >>= bottomUpM doInclude . readMarkdown def
                   >>= bottomUpM doHtml
                   >>= putStrLn . writeMarkdown def
