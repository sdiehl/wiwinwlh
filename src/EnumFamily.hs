module EnumFamily where
import Language.Haskell.TH

enumFamily :: (Integer -> Integer -> Integer)
           -> Name
           -> Integer
           -> Q [Dec]
enumFamily f bop upper = return decls
  where
    decls = do
      i <- [1..upper]
      j <- [2..upper]
      return $ TySynInstD bop (rhs i j)

    rhs i j = TySynEqn
      [LitT (NumTyLit i), LitT (NumTyLit j)]
      (LitT (NumTyLit (i `f` j)))
