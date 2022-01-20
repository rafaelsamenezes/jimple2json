module Parser.Expression (jimpleExpression, jimpleFieldAccessExpression, jimpleNewArrayExpression, jimpleBoolExpr, jimpleDereferenceExpression) where

import Ast
    ( New(Simple),
      BinOp(..),
      Expression(..),
      Immediate(Local) )
import Lexer
  ( atIdentifier,
    dotSep,
    fullIdentifier,
    identifier,
    lexer,
    integer,
    reserved,
    reservedOp,
  )
import Text.Parsec
  ( ParseError,
    char,
    digit,
    eof,
    many,
    many1,
    noneOf,
    parse,
    try,
    (<|>),
  )
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import qualified Text.ParserCombinators.Parsec.Combinator as Tok
import Parser.Type (jimpleType)
import Parser.ClassName ( jimpleClassName ) 
import Parser.Immediate ( jimpleImmediate )
import Parser.Invoke (jimpleInvokeExpr )

-- <kotlin._Assertions: boolean ENABLED>
jimpleFieldAccessExpression :: Parser Expression
jimpleFieldAccessExpression = do
  reservedOp "<"
  baseclass <- jimpleClassName
  reservedOp ":"
  castType <- jimpleType
  name <- identifier
  reservedOp ">"
  return $ FieldAccess baseclass name castType

jimpleBinOp = try (reservedOp "==" >> return CmpEq)
  <|> try (reservedOp "!=" >> return CmpNe)
  <|> try (reservedOp ">=" >> return CmpGEq)
  <|> try (reservedOp "-" >> return Minus)
  <|> try (reservedOp "+" >> return Add)
  <|> try (reservedOp ">" >> return CmpG)


jimpleBinaryExpression :: Parser Expression
jimpleBinaryExpression = do
  lhs <- jimpleImmediate
  Tok.whiteSpace lexer
  op <- jimpleBinOp
  Tok.whiteSpace lexer
  rhs <- jimpleImmediate
  Tok.whiteSpace lexer
  return $ BinOp lhs rhs op


jimpleBoolExpr :: Parser Expression
jimpleBoolExpr = do
  lhs <- jimpleImmediate
  Tok.whiteSpace lexer
  op <- jimpleBinOp
  Tok.whiteSpace lexer
  rhs <- jimpleImmediate
  Tok.whiteSpace lexer
  return $ BinOp lhs rhs op

jimpleDereferenceExpression :: Parser Expression
jimpleDereferenceExpression = do
  base <-  identifier
  index <- Tok.brackets lexer jimpleImmediate
  return $ Dereference (Local base) index

jimpleNewArrayExpression :: Parser Expression
jimpleNewArrayExpression = do
  reserved "newarray"
  base <-  Tok.parens lexer $ jimpleType
  size <- Tok.brackets lexer $ jimpleImmediate
  return $ NewArray base size



jimpleNew :: Parser New
jimpleNew = do
  reserved "new"
  Simple <$> jimpleType

jimpleExpression :: Parser Expression
jimpleExpression =
  try jimpleFieldAccessExpression
    <|> try jimpleDereferenceExpression
    <|> try jimpleNewArrayExpression
    <|> try (New <$> jimpleNew)
    <|> try (InvokeExpr <$> jimpleInvokeExpr)
    <|> try jimpleBinaryExpression
    <|> try (Immediate <$> jimpleImmediate)