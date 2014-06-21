import Control.Monad
import Text.PrettyPrint
import qualified Data.Map as Map

-- de Bruijn indices
data DExp
  = Var Integer
  | Lam DExp
  | App DExp DExp
  deriving (Eq)

subst :: DExp -> Integer -> DExp -> DExp
subst e n (Var n')
  | n == n' = e
  | otherwise = (Var n')
subst e n (Lam e') = Lam $ subst e (n+1) e'
subst e n (App e1 e2) = App (subst e n e1) (subst e n e2)

nf :: DExp -> DExp
nf e@(Var _) = e
nf (Lam e) = Lam (nf e)
nf (App f a) =
  case whnf f of
      Lam b -> nf (subst a 0 b)
      f' -> App (nf f') (nf a)

whnf :: DExp -> DExp
whnf e@(Var _) = e
whnf e@(Lam _) = e
whnf (App f a) =
  case whnf f of
      Lam b -> whnf (subst a 0 b)
      f' -> App f' a


-- Pretty printer

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty DExp where
    ppr _ (Var v)   = integer (v+1)
    ppr p (Lam f)   = parensIf (p>0) $ text "λ " <> ppr p f
    ppr p (App f x) = ppr' f <+> ppr' x
      where
        ppr' (Var v) = integer (v+1)
        ppr' expr    = parens $ ppr p expr

ppexpr :: DExp -> String
ppexpr = render . ppr 0

-- Locally named
data NExp
  = EVar String
  | ELam String NExp
  | EApp NExp NExp
  deriving (Show)

type Ctx = Map.Map String Integer

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

shift :: Ctx -> NExp -> DExp
shift c (EVar v) = Var (c Map.! v)
shift c (EApp a b) = App (shift c a) (shift c b)
shift c (ELam v body) = Lam (shift c' body)
  where c' = Map.insert v 0 (Map.map (+1) c)

toDeBruijn :: NExp -> DExp
toDeBruijn = shift Map.empty

fromDeBruijn :: DExp -> NExp
fromDeBruijn = from 0
  where from n (Var i)   = EVar (letters !! (n - (fromIntegral i) - 1))
        from n (Lam b)   = ELam (letters !! n) (from (succ n) b)
        from n (App f a) = EApp (from n f) (from n a)

i = ELam "a" (EVar "a")
k = ELam "a" (ELam "b" (EVar "a"))
s = ELam "a" (ELam "b" (ELam "c" (EApp (EApp (EVar "a") (EVar "c")) (EApp (EVar "b") (EVar "c")))))

ex1 = ppexpr $ toDeBruijn i
-- λ 1
ex2 = ppexpr $ toDeBruijn k
-- λ λ 2
ex3 = ppexpr $ toDeBruijn s
-- λ λ λ (3 1) (2 1)
ex4 = fromDeBruijn $ toDeBruijn s
-- ELam "a" (ELam "b" (ELam "c" (EApp (EApp (EVar "a") (EVar "c")) (EApp (EVar "b") (EVar "c")))))
