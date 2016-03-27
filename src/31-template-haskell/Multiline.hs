{-# LANGUAGE TemplateHaskell #-}

module Multiline (s) where

import Data.String
import Language.Haskell.TH.Quote

s :: QuasiQuoter
s = QuasiQuoter
  { quoteExp  = (\a -> [|fromString a|]) . trim
  , quotePat  = \_ -> fail "illegal raw string QuasiQuote"
  , quoteType = \_ -> fail "illegal raw string QuasiQuote"
  , quoteDec  = \_ -> fail "illegal raw string QuasiQuote"
  }

trim :: String -> String
trim ('\n':xs) = xs
trim xs = xs
