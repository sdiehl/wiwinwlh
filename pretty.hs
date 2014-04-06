{-# LANGUAGE FlexibleInstances #-}

import Text.PrettyPrint
import Text.Show.Pretty (ppShow)

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

type Name = String

data Expr
  = Var String
  | Lit Ground
  | App Expr Expr
  | Lam Name Expr
  deriving (Eq, Show)

data Ground
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)


class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty String where
  ppr _ x = text x

instance Pretty Expr where
  ppr _ (Var x)         = text x
  ppr _ (Lit (LInt a))  = text (show a)
  ppr _ (Lit (LBool b)) = text (show b)

  ppr p e@(App _ _) =
    let (f, xs) = viewApp e in
    let args = sep $ map (ppr (p+1)) xs in
    parensIf (p>0) $ ppr p f <+> args

  ppr p e@(Lam _ _) =
    let body = ppr (p+1) (viewBody e) in
    let vars = map (ppr 0) (viewVars e) in
    parensIf (p>0) $ char '\\' <> hsep vars <+> text "." <+> body

viewVars :: Expr -> [Name]
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

ppexpr :: Expr -> String
ppexpr = render . ppr 0


s, k, example :: Expr
s = Lam "f" (Lam "g" (Lam "x" (App (Var "f") (App (Var "g") (Var "x")))))
k = Lam "x" (Lam "y" (Var "x"))
example = App s k

main :: IO ()
main = do
  putStrLn $ ppexpr s
  putStrLn $ ppShow example
