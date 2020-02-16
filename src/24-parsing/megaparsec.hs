{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Combinators
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Expr Text

data Expr
  = Var Char
  | Lam Char Expr
  | App Expr Expr
  deriving (Eq, Ord, Show)

instance ShowErrorComponent Expr where
  showErrorComponent = show

lam :: Parser Expr
lam = do
  char '\\'
  n <- letterChar
  string "->"
  e <- expr
  return $ Lam n e

app :: Parser Expr
app = do
  apps <- many term
  return $ foldl1 App apps

var :: Parser Expr
var = do
  n <- letterChar
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

example :: Text
example = "\\y->y(\\x->x)y"

main :: IO ()
main = case parse decl "<stdin>" example of
  Left bundle -> putStr (errorBundlePretty bundle)
  Right result -> print result
