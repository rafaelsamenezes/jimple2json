module Parser.Type (jimpleType) where
import Ast ( NonVoidType(BaseType), BaseType(..), Type(..) )
import Text.Parsec.String (Parser)
import Lexer ( reserved )
import Parser.ClassName ( jimpleClassName )
import Text.Parsec ( (<|>), try, many )

jimpleTypeVoid :: Parser Type
jimpleTypeVoid = do
  reserved "void"
  return Void

jimpleNonVoidBoolean :: Parser BaseType
jimpleNonVoidBoolean = do
  reserved "boolean"
  return Boolean

jimpleNonVoidByte :: Parser BaseType
jimpleNonVoidByte = do
  reserved "byte"
  return Byte

jimpleNonVoidChar :: Parser BaseType
jimpleNonVoidChar = do
  reserved "char"
  return Char

jimpleNonVoidShort :: Parser BaseType
jimpleNonVoidShort = do
  reserved "short"
  return Short

jimpleNonVoidInt :: Parser BaseType
jimpleNonVoidInt = do
  reserved "int"
  return Int

jimpleNonVoidLong :: Parser BaseType
jimpleNonVoidLong = do
  reserved "long"
  return Long

jimpleNonVoidFloat :: Parser BaseType
jimpleNonVoidFloat = do
  reserved "float"
  return Float

jimpleNonVoidDouble :: Parser BaseType
jimpleNonVoidDouble = do
  reserved "double"
  return Double

jimpleNonVoidNullType :: Parser BaseType
jimpleNonVoidNullType = do
  reserved "null_type"
  return NullType

jimpleNonVoidClassName :: Parser BaseType
jimpleNonVoidClassName = do
  ClassName <$> jimpleClassName

jimpleTypeNonVoidBase :: Parser BaseType
jimpleTypeNonVoidBase =
  try jimpleNonVoidBoolean
    <|> try jimpleNonVoidByte
    <|> try jimpleNonVoidChar
    <|> try jimpleNonVoidShort
    <|> try jimpleNonVoidInt
    <|> try jimpleNonVoidLong
    <|> try jimpleNonVoidFloat
    <|> try jimpleNonVoidDouble
    <|> try jimpleNonVoidNullType
    <|> try jimpleNonVoidClassName

jimpleArrayBrackets = do
  reserved "["
  reserved "]"

jimpleTypeNonVoidBaseFull :: Parser NonVoidType
jimpleTypeNonVoidBaseFull = do
  base <- jimpleTypeNonVoidBase
  value <- many jimpleArrayBrackets
  return $ BaseType base $ length value

jimpleTypeNonVoid :: Parser Type
jimpleTypeNonVoid = do
  NonvoidType <$> jimpleTypeNonVoidBaseFull

jimpleType :: Parser Type
jimpleType =
  try jimpleTypeVoid
    <|> try jimpleTypeNonVoid



