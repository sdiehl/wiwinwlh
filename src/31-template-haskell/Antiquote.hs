{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Antiquote where

import Data.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

data Expr
  = Tr
  | Fl
  | Zero
  | Succ Expr
  | Pred Expr
  | Antiquote String
  deriving (Eq, Show, Data, Typeable)

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

identifier :: Parser String
identifier = Tok.identifier lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

oper s f assoc = Ex.Prefix (reservedOp s >> return f)

table = [ oper "succ" Succ Ex.AssocLeft
        , oper "pred" Pred Ex.AssocLeft
        ]

expr :: Parser Expr
expr = Ex.buildExpressionParser [table] factor

true, false, zero :: Parser Expr
true  = reserved "true" >> return Tr
false = reserved "false" >> return Fl
zero  = reservedOp "0" >> return Zero

antiquote :: Parser Expr
antiquote = do
  char '$'
  var <- identifier
  return $ Antiquote var

factor :: Parser Expr
factor = true
      <|> false
      <|> zero
      <|> antiquote
      <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s


class Expressible a where
  express :: a -> Expr

instance Expressible Expr where
  express = id

instance Expressible Bool where
  express True = Tr
  express False = Fl

instance Expressible Integer where
  express 0 = Zero
  express n = Succ (express (n - 1))


exprE :: String -> Q Exp
exprE s = do
  filename <- loc_filename `fmap` location
  case parse (contents expr) filename s of
    Left err -> error (show err)
    Right exp -> dataToExpQ (const Nothing `extQ` antiExpr) exp

exprP :: String -> Q Pat
exprP s = do
  filename <- loc_filename `fmap` location
  case parse (contents expr) filename s of
    Left err -> error (show err)
    Right exp -> dataToPatQ (const Nothing `extQ` antiExprPat) exp

-- antiquote RHS
antiExpr :: Expr -> Maybe (Q Exp)
antiExpr (Antiquote v) = Just embed
  where embed = [| express $(varE (mkName v)) |]
antiExpr _ = Nothing

-- antiquote LHS
antiExprPat :: Expr -> Maybe (Q Pat)
antiExprPat (Antiquote v) = Just $ varP (mkName v)
antiExprPat _ = Nothing

mini :: QuasiQuoter
mini = QuasiQuoter exprE exprP undefined undefined
