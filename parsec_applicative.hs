import Control.Applicative

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellStyle)

import qualified Text.Parsec.Token as Tok

data Expr = Add String String deriving Show

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = ["->","\\","+","*","-","="]
        style = haskellStyle {Tok.reservedOpNames = ops }

identifier :: Parser String
identifier = Tok.identifier lexer

parseM :: Parser Expr
parseM = do
  a <- identifier
  char '+'
  b <- identifier
  return $ Add a b

parseA :: Parser Expr
parseA = Add <$> identifier <* char '+' <*> identifier

main :: IO ()
main = do
  s0 <- getLine
  print $ parse parseM "<stdin>" s0
  s1 <- getLine
  print $ parse parseA "<stdin>" s1
