module Parser.Immediate (jimpleImmediate) where

import Ast ( Immediate(..) )
import Lexer ( identifier, reserved, lexer )
import Text.Parsec ( (<|>), try, noneOf, many1, many, digit, char )
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

jimpleImmediateValue :: Parser Immediate
jimpleImmediateValue = do
  value <- many1 digit
  _ <- many $ char 'L'
  return $ Value value

jimpleImmediateNegativeValue :: Parser Immediate
jimpleImmediateNegativeValue = do
  char '-'
  value <- many1 digit
  return $ Value ('-':value)

jimpleImmediateString :: Parser Immediate
jimpleImmediateString = do
  char '"'
  value <- many $ noneOf ['"']
  char '"'
  return $ StringConst value

t = Local <$> Tok.angles lexer identifier
y = Local <$> identifier

jimpleImmediateLocal :: Parser Immediate
jimpleImmediateLocal = try y <|> try t
  

jimpleImmediateClzz :: Parser Immediate
jimpleImmediateClzz = do
  reserved "class"
  Tok.whiteSpace lexer
  char '"'
  value <- many $ noneOf ['"']
  char '"'
  return $ Clzz value





jimpleImmediate :: Parser Immediate
jimpleImmediate =
  try jimpleImmediateValue
    <|> try jimpleImmediateNegativeValue
    <|> try jimpleImmediateClzz
    <|> try jimpleImmediateString
    <|> try jimpleImmediateLocal
