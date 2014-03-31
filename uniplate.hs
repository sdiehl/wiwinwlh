{-
plate :: from -> Type from to
(|*) :: Type (to -> from) to -> to -> Type from to
(|-) :: Type (item -> from) to -> item -> Type from to
-}

import Data.Generics.Uniplate.Direct

data Expr a
  = Fls
  | Tru
  | Lit a
  | Not (Expr a)
  | And (Expr a) (Expr a)
  | Or (Expr a) (Expr a)
  deriving (Show, Eq)

instance Uniplate (Expr a) where
  uniplate (Not f)     = plate Not |* f
  uniplate (And f1 f2) = plate And |* f1 |* f2
  uniplate (Or f1 f2)  = plate Or |* f1 |* f2
  uniplate x           = plate x

{-
import Data.Data
import Data.Typeable
import Data.Generics.Uniplate.Data

data Expr a
  = Fls
  | Tru
  | Lit a
  | Not (Expr a)
  | And (Expr a) (Expr a)
  | Or (Expr a) (Expr a)
  deriving (Data, Typeable, Show, Eq)
-}


simplify :: Expr a -> Expr a
simplify = transform simp
 where
   simp (Not (Not f)) = f
   simp (Not Fls) = Tru
   simp (Not Tru) = Fls
   simp x = x

test =
  simplify (Not (Not (Not (Not (Lit "a"))))) == Lit "a"
