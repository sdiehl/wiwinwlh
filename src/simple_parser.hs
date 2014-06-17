import Text.Parsec
import Text.Parsec.String

data Expr
  = Var Char
  | Lam Char Expr
  | App Expr Expr
  deriving Show

lam :: Parser Expr
lam = do
  char '\\'
  n <- letter
  string "->"
  e <- expr
  return $ Lam n e

app :: Parser Expr
app = do
  apps <- many1 term
  return $ foldl1 App apps

var :: Parser Expr
var = do
  n <- letter
  return $ Var n

parens :: Parser Expr -> Parser Expr
parens p = do
  char '('
  e <- p
  char ')'
  return e

term :: Parser Expr
term = var <|> parens expr

expr :: Parser Expr
expr = lam <|> app

decl :: Parser Expr
decl = do
  e <- expr
  eof
  return e

test :: IO ()
test = parseTest decl "\\y->y(\\x->x)y"

main :: IO ()
main = test >>= print
