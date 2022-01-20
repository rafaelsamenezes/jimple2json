module Parser.Modifier (jimpleModifier) where

import Text.Parsec.String (Parser)
import Ast ( Modifier(..) )
import Lexer ( reserved ) 
import Text.Parsec ( (<|>), try )

jimpleModifierAbstract :: Parser Modifier
jimpleModifierAbstract = do
  reserved "abstract"
  return Abstract

jimpleModifierFinal :: Parser Modifier
jimpleModifierFinal = do
  reserved "final"
  return Final

jimpleModifierNative :: Parser Modifier
jimpleModifierNative = do
  reserved "native"
  return Native

jimpleModifierPublic :: Parser Modifier
jimpleModifierPublic = do
  reserved "public"
  return Public

jimpleModifierProtected :: Parser Modifier
jimpleModifierProtected = do
  reserved "protected"
  return Protected

jimpleModifierPrivate :: Parser Modifier
jimpleModifierPrivate = do
  reserved "private"
  return Private

jimpleModifierStatic :: Parser Modifier
jimpleModifierStatic = do
  reserved "static"
  return Static

jimpleModifierSynchronized :: Parser Modifier
jimpleModifierSynchronized = do
  reserved "synchronized"
  return Synchronized

jimpleModifierTransient :: Parser Modifier
jimpleModifierTransient = do
  reserved "transient"
  return Transient

jimpleModifierVolatile :: Parser Modifier
jimpleModifierVolatile = do
  reserved "volatile"
  return Volatile

jimpleModifierStrictFp :: Parser Modifier
jimpleModifierStrictFp = do
  reserved "strictfp"
  return StrictFp

jimpleModifierEnum :: Parser Modifier
jimpleModifierEnum = do
  reserved "enum"
  return Enum

jimpleModifierAnnotation :: Parser Modifier
jimpleModifierAnnotation = do
  reserved "annotation"
  return Annotation

jimpleModifier :: Parser Modifier
jimpleModifier =
  try jimpleModifierAbstract
    <|> try jimpleModifierFinal
    <|> try jimpleModifierNative
    <|> try jimpleModifierPublic
    <|> try jimpleModifierProtected
    <|> try jimpleModifierPrivate
    <|> try jimpleModifierStatic
    <|> try jimpleModifierSynchronized
    <|> try jimpleModifierTransient
    <|> try jimpleModifierVolatile
    <|> try jimpleModifierStrictFp
    <|> try jimpleModifierEnum
    <|> try jimpleModifierAnnotation
