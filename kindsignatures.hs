{-# Language GADTs #-}
{-# LANGUAGE KindSignatures #-}

data Term a :: * where
  Lit    :: a -> Term a
  Succ   :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If     :: Term Bool -> Term a -> Term a -> Term a

data Vec :: * -> * -> * where
  Nil :: Vec n a
  Cons :: a -> Vec n a -> Vec n a

data Fix :: (* -> *) -> * where
  In :: f (Fix f) -> Fix f
