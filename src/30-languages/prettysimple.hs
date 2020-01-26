import Text.Pretty.Simple

main :: IO ()
main = do
  pPrint [1 .. 25]
  pPrint [Just (1, "hello")]
