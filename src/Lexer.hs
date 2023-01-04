module Lexer where

import Text.Parsec
  ( alphaNum,
    char,
    digit,
    letter,
    many,
    many1,
    sepBy,
    sepBy1,
    string,
    try,
    (<|>),
  )
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Token (GenTokenParser (commaSep))
import Data.List (intercalate, intersperse)

-- Lexer Analyser generation
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+", "-", "*", "<=", "==", ">=", ">", "<", "=", ";", ":", "'", ":=", ".", "[", "]"]
    names =
      [ "abstract",
        "final",
        "native",
        "public",
        "protected",
        "private",
        "static",
        "synchronized",
        "transient",
        "volatile",
        "strictfp",
        "enum",
        "annotation",
        "class",
        "interface",
        "void",
        "boolean",
        "byte",
        "short",
        "char",
        "int",
        "long",
        "float",
        "double",
        "null_type",
        "unknown",
        "extends",
        "implements",
        "breakpoint",
        "case",
        "catch",
        "cmp",
        "cmpg",
        "cmpl",
        "default",
        "entermonitor",
        "exitmonitor",
        "goto",
        "if",
        "instanceof",
        "interfaceinvoke",
        "lengthof",
        "lookupswitch",
        "neg",
        "new",
        "newarray",
        "newmultiarray",
        "nop",
        "ret",
        "return",
        "specialinvoke",
        "dynamicinvoke",
        "tableswitch",
        "throw",
        "throws",
        "virtualinvoke",
        "null",
        "from",
        "to",
        "with",
        "this:"
      ]
    style =
      emptyDef
        { Tok.commentLine = "//",
          Tok.reservedOpNames = ops,
          Tok.reservedNames = names
        }

symbol = Tok.symbol lexer

dotSep p = p `sepBy` symbol "."
dotSep1 p = p `sepBy1` symbol "."

integer :: Parser Integer
integer = Tok.integer lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

simpleIdChar :: Parser Char
simpleIdChar =
  try alphaNum
    <|> try (char '_' >> return '_')
    <|> try (char '$' >> return '$')
    <|> try (char '-' >> return '-')
    <|> try (char '?' >> return '?')

firstIdChar =
  try letter
    <|> try (char '_' >> return '_')
    <|> try (char '$' >> return '$')
    <|> try escapeChar

escapeChar :: Parser Char
escapeChar =
  try (char '\\' >> return '\\')
    <|> try (char '\'' >> return '\'')
    <|> try (char '\"' >> return '\"')
    -- <|> try (char '\n' >> return '\n')
    <|> try (char '\t' >> return '\t')
    <|> try (char '\r' >> return '\r')
    <|> try (char '\b' >> return '\f')

fullIdentifierBase :: Parser String
fullIdentifierBase = do
  var <- try firstIdChar <|> try (char '\'' >> return '\'')
  rest <- many $ try simpleIdChar <|> try escapeChar
  return (var : rest)

fullIdentifier :: Parser String
fullIdentifier = do
  names <- dotSep1 fullIdentifierBase
  Tok.whiteSpace lexer
  return $ intercalate "." names

identifierBase :: Parser String
identifierBase = do
  var <- firstIdChar
  rest <- many $ try simpleIdChar <|> try escapeChar
  Tok.whiteSpace lexer
  return (var : rest)

identifier :: Parser String
identifier =
  try identifierBase
    <|> try (string "<clinit>" >> return "<clinit>")
    <|> try (string "<init>" >> return "<init>")

atIdentifierParameter :: Parser String
atIdentifierParameter = do
  string "parameter"
  val <- many1 digit
  reservedOp ":"
  return $ "parameter" ++ val

--at_identifier = '@' (('parameter' dec_digit+ ':') | 'this' ':' | 'caughtexception');
atIdentifier :: Parser String
atIdentifier = do
  reservedOp "@"
  name <- try atIdentifierParameter <|> try (string "this:" >> return "this") <|> try (reserved "caughtexception" >> return "caughtexception")
  Tok.whiteSpace lexer
  return name

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
