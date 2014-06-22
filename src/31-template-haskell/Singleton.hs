{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Singleton where

import Text.Read
import Language.Haskell.TH
import Language.Haskell.TH.Quote

data Nat = Z | S Nat

data SNat :: Nat -> * where
  SZero :: SNat Z
  SSucc :: SNat n -> SNat (S n)

-- Quasiquoter for Singletons

sval :: String -> Q Exp
sval str = do
  case readEither str of
    Left err -> fail (show err)
    Right n -> do
      Just suc <- lookupValueName "SSucc"
      Just zer <- lookupValueName "SZero"
      return $ foldr AppE (ConE zer) (replicate n (ConE suc))

stype :: String -> Q Type
stype str = do
  case readEither str of
    Left err -> fail (show err)
    Right n -> do
      Just scon <- lookupTypeName "SNat"
      Just suc <- lookupValueName "S"
      Just zer <- lookupValueName "Z"
      let nat = foldr AppT (PromotedT zer) (replicate n (PromotedT suc))
      return $ AppT (ConT scon) nat

spat :: String -> Q Pat
spat str = do
  case readEither str of
    Left err -> fail (show err)
    Right n -> do
      Just suc <- lookupValueName "SSucc"
      Just zer <- lookupValueName "SZero"
      return $ foldr (\x y -> ConP x [y]) (ConP zer []) (replicate n (suc))

sdecl :: String -> a
sdecl _ = error "Cannot make toplevel declaration for snat."

snat :: QuasiQuoter
snat = QuasiQuoter sval spat stype sdecl
