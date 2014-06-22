module Parser (parseExpr) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellStyle)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

type Id = String

data Expr
  = Lam Id Expr
  | App Expr Expr
  | Var Id
  | Num Int
  | Op  Binop Expr Expr
  deriving (Show)

data Binop = Add | Sub | Mul deriving Show

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = ["->","\\","+","*","-","="]
        style = haskellStyle {Tok.reservedOpNames = ops }

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

natural :: Parser Integer
natural = Tok.natural lexer

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

number :: Parser Expr
number = do
  n <- natural
  return (Num (fromIntegral n))

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  x <- identifier
  reservedOp "->"
  e <- expr
  return (Lam x e)

aexp :: Parser Expr
aexp =  parens expr
    <|> variable
    <|> number
    <|> lambda

term :: Parser Expr
term = Ex.buildExpressionParser table aexp
  where infixOp x f = Ex.Infix (reservedOp x >> return f)
        table = [[infixOp "*" (Op Mul) Ex.AssocLeft],
                 [infixOp "+" (Op Add) Ex.AssocLeft]]

expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 App es)

parseExpr :: String -> Expr
parseExpr input =
  case parse (contents expr) "<stdin>" input of
    Left err -> error (show err)
    Right ast -> ast

main :: IO ()
main = getLine >>= print . parseExpr >> main
