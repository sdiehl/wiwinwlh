{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Class where

import Language.Haskell.TH

class Pretty a where
  ppr :: a -> String

normalCons :: Con -> Name
normalCons (NormalC n _) = n

getCons :: Info -> [Name]
getCons cons = case cons of
    TyConI (DataD    _ _ _ tcons _) -> map normalCons tcons
    con -> error $ "Can't derive for:" ++ (show con)

pretty :: Name -> Q [Dec]
pretty dt = do
  info <- reify dt
  Just cls <- lookupTypeName "Pretty"
  let datatypeStr = nameBase dt
  let cons = getCons info
  let dtype = mkName (datatypeStr)
  let mkInstance xs =
        InstanceD
        []                              -- Context
        (AppT
          (ConT cls)                    -- Instance
          (ConT dtype))                 -- Head
        [(FunD (mkName "ppr") xs)]      -- Methods
  let methods = map cases cons
  return $ [mkInstance methods]

-- Pattern matches on the ``ppr`` method
cases :: Name -> Clause
cases a = Clause [ConP a []] (NormalB (LitE (StringL (nameBase a)))) []
