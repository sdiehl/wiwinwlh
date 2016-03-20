{-# LANGUAGE OverloadedStrings #-}

import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang

import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Expr
  = Var T.Text
  | App Expr Expr
  | Lam T.Text Expr
  deriving (Show)

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = Tok.opLetter style
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedOpNames = []
  , Tok.reservedNames   = []
  , Tok.caseSensitive   = True
  }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer (T.unpack op)

ident :: Parser T.Text
ident = T.pack <$> Tok.identifier lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

var :: Parser Expr
var = do
  var <- ident
  return (Var var )

app :: Parser Expr
app = do
  e1 <- expr
  e2 <- expr
  return (App e1 e2)

fun :: Parser Expr
fun = do
  reservedOp "\\"
  binder <- ident
  reservedOp "."
  rhs <- expr
  return (Lam binder rhs)

expr :: Parser Expr
expr = do
  es <- many1 aexp
  return (foldl1 App es)

aexp :: Parser Expr
aexp = fun <|> var <|> (parens expr)

test :: T.Text -> Either ParseError Expr
test = parse (contents expr) "<stdin>"

repl :: IO ()
repl = do
  str <- TIO.getLine
  print (test str)
  repl

main :: IO ()
main = repl
