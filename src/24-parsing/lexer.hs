import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

lexerStyle :: Token.LanguageDef ()
lexerStyle = Token.LanguageDef
  { Token.commentStart   = "{-"
  , Token.commentEnd     = "-}"
  , Token.commentLine    = "--"
  , Token.nestedComments = True
  , Token.identStart     = letter
  , Token.identLetter    = alphaNum <|> oneOf "_"
  , Token.opStart        = Token.opLetter lexerStyle
  , Token.opLetter       = oneOf "`~!@$%^&*-+=;:<>./?"
  , Token.reservedOpNames= []
  , Token.reservedNames  = ["if", "then", "else", "def"]
  , Token.caseSensitive  = True
  }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser lexerStyle

parens :: Parser a -> Parser a
parens = Token.parens lexer

natural :: Parser Integer
natural = Token.natural lexer

identifier :: Parser String
identifier = Token.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

comma :: Parser String
comma = Token.comma lexer
