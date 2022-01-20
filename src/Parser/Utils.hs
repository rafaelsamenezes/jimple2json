module Parser.Utils where

import Parser.Type (jimpleType)
import Parser.ClassName ( jimpleClassName ) 
import Parser.Immediate (jimpleImmediate)
import Ast 
import Lexer 
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

jimpleParameter :: Parser ParameterList
jimpleParameter = do
  Tok.parens lexer $ Tok.commaSep lexer jimpleType

hackParameter :: Parser [Immediate]
hackParameter = do
  Tok.parens lexer $ Tok.commaSep lexer jimpleImmediate

jimpleMethodSignature :: Parser MethodSignature
jimpleMethodSignature = do
  char '<'
  name <- jimpleClassName
  reservedOp ":"
  type_ <- jimpleType
  method_name <- identifier
  parameters <- jimpleParameter
  char '>'
  return $ MethodSignature name type_ method_name parameters

