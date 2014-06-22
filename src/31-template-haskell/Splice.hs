{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Splice where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

spliceF :: Q [Dec]
spliceF = do
  let f = mkName "f"
  a <- newName "a"
  b <- newName "b"
  return [ FunD f [ Clause [VarP a, VarP b] (NormalB (VarE a)) [] ] ]

spliceG :: Lift a => a -> Q [Dec]
spliceG n = runQ [d| g a = n |]
