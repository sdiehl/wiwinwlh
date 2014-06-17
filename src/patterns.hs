{-# LANGUAGE PatternSynonyms #-}

import Data.List (foldl1')

type Name  = String
type TVar  = String
type TyCon = String

data Type
  = TVar TVar
  | TCon TyCon
  | TApp Type Type
  deriving (Show, Eq, Ord)


pattern TArr t1 t2 = TApp (TApp (TCon "(->)") t1) t2

tapp :: TyCon -> [Type] -> Type
tapp tcon args = foldl TApp (TCon tcon) args

arr :: [Type] -> Type
arr ts = foldl1' (\t1 t2 -> tapp "(->)" [t1, t2]) ts

elimTArr :: Type -> [Type]
elimTArr (TArr (TArr t1 t2) t3) = t1 : t2 : elimTArr t3
elimTArr (TArr t1 t2) = t1 : elimTArr t2
elimTArr t = [t]

-- (->) a ((->) b a)
-- a -> b -> a
to :: Type
to = arr [TVar "a", TVar "b", TVar "a"]

from :: [Type]
from = elimTArr to
