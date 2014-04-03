-- includes.hs
import Text.Pandoc

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
       Just f     -> return . (CodeBlock (id, classes, namevals)) =<< readFile f
       Nothing    -> return cb
doInclude x = return x

main :: IO ()
main = getContents >>= bottomUpM doInclude . readMarkdown def
                   >>= putStrLn . writeMarkdown def
