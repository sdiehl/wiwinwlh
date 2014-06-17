{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Text.Show.Pretty (ppShow)
import Language.Haskell.TH

introspect :: Name -> Q Exp
introspect n = do
  t <- reify n
  runIO $ putStrLn $ ppShow t
  [| return () |]
