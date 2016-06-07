{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as TL
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>), Pretty(..))
import qualified Text.PrettyPrint.Leijen.Text as PP

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

data Expr
  = Var Text
  | Lit Lit
  | App Expr Expr
  | Lam Text Expr

data Lit
  = LInt Int
  | LBool Bool

class Pretty a where
  pretty :: Int -> a -> Doc

instance Pretty Lit where
  pretty _ (LInt n) = int n
  pretty _ (LBool b) = bool b

instance Pretty Expr where
  pretty _ (Var x)  = text x
  pretty p (Lit x)  = pretty p x

  pretty p e@(App _ _) =
    let (f, xs) = viewApp e in
    let args = sep $ map (pretty (p+1)) xs in
    parensIf (p>0) $ pretty p f <+> args

  pretty p e@(Lam _ _) =
    let body = pretty (p+1) (viewBody e) in
    let vars = map text (viewVars e) in
    parensIf (p>0) $ "\\" <> hsep vars <+> "." <+> body

viewVars :: Expr -> [Text]
viewVars (Lam n a) = n : viewVars a
viewVars _ = []

viewBody :: Expr -> Expr
viewBody (Lam _ a) = viewBody a
viewBody x = x

viewApp :: Expr -> (Expr, [Expr])
viewApp (App e1 e2) = go e1 [e2]
  where
    go (App a b) xs = go a (b : xs)
    go f xs = (f, xs)
viewApp x = (x, [])

ppexpr :: Expr -> Text
ppexpr x = PP.displayT (PP.renderPretty 1.0 70 (pretty 0 x))

s, k, example :: Expr
s = Lam "f" (Lam "g" (Lam "x" (App (Var "f") (App (Var "g") (Var "x")))))
k = Lam "x" (Lam "y" (Var "x"))
example = App s k

main :: IO () main = do
  TL.putStrLn (ppexpr s)
  TL.putStrLn (ppexpr k)
