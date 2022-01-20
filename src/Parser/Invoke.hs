module Parser.Invoke (jimpleInvokeExpr) where

import Text.Parsec.String (Parser)
import Ast 
import Lexer
import Parser.Utils
import Text.Parsec 


jimpleStatementSpecialInvoke :: Parser InvokeExpr
jimpleStatementSpecialInvoke = do
  reserved "specialinvoke"
  var <- identifier
  char '.'
  signature <- jimpleMethodSignature
  parameters <- hackParameter
  return $ SpecialInvoke var signature []

jimpleStatementStaticInvoke :: Parser InvokeExpr
jimpleStatementStaticInvoke = do
  reserved "staticinvoke"
  signature <- jimpleMethodSignature
  parameters <- hackParameter
  return $ StaticInvoke signature parameters

jimpleInvokeExpr :: Parser InvokeExpr
jimpleInvokeExpr =
    try jimpleStatementStaticInvoke
    <|> try jimpleStatementSpecialInvoke
    -- <|> try jimpleStatementVirtualInvoke