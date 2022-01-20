module Parser.Immediate (jimpleImmediate) where

import Ast ( Immediate(..) )
import Lexer ( identifier )
import Text.Parsec ( (<|>), try, noneOf, many1, many, digit, char )
import Text.Parsec.String (Parser)

jimpleImmediateValue :: Parser Immediate 
jimpleImmediateValue = do
  value <- many1 digit
  return $ Value value

jimpleImmediateString :: Parser Immediate 
jimpleImmediateString = do
  char '"'
  value <- many $ noneOf ['"']
  char '"'
  return $ StringConst value

jimpleImmediateLocal :: Parser Immediate 
jimpleImmediateLocal = do
  Local <$> identifier

jimpleImmediate :: Parser Immediate 
jimpleImmediate =
  try jimpleImmediateValue
    <|> try jimpleImmediateString
    <|> try jimpleImmediateLocal