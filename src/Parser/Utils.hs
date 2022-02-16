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

jimpleFieldSignature :: Parser FieldSignature
jimpleFieldSignature = do
  reservedOp "<"
  baseclass <- jimpleClassName
  reservedOp ":"
  castType <- jimpleType
  name <- identifier
  reservedOp ">"
  return $ FieldSignature baseclass castType name

jimpleFieldSignatureRef :: Parser FieldReference
jimpleFieldSignatureRef = FieldSignatureRef <$> jimpleFieldSignature

jimpleLocalFieldSignature :: Parser FieldReference
jimpleLocalFieldSignature = do
  name <- identifier
  char '.'
  LocalFieldReference name <$> jimpleFieldSignature

jimpleFieldReference :: Parser FieldReference
jimpleFieldReference = try jimpleFieldSignatureRef
                    <|> try jimpleLocalFieldSignature

jimpleArrayRefExpression :: Parser Reference
jimpleArrayRefExpression = do
  base <-  identifier
  index <- Tok.brackets lexer jimpleImmediate
  return $ ArrayRef base index

jimpleReference :: Parser Reference
jimpleReference = try $ FieldRef <$> jimpleFieldReference
               <|> try jimpleArrayRefExpression


jimpleVariableLocal :: Parser Variable
jimpleVariableLocal = do
  LocalName <$> identifier

jimpleVariableRef :: Parser Variable
jimpleVariableRef = do
  Reference <$> jimpleReference

jimpleVariable :: Parser Variable
jimpleVariable = try jimpleVariableRef
              <|> try jimpleVariableLocal