module Parser.Parser where

import Parser.Immediate (jimpleImmediate)
import Parser.Modifier (jimpleModifier)
import Parser.ClassName (jimpleClassName)
import Parser.Type (jimpleType)
import  Parser.Expression (jimpleBoolExpr, jimpleExpression)
import Parser.Invoke ( jimpleInvokeExpr )
import Parser.Utils
import Ast
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
    string,
    manyTill,
    anyChar,
    anyToken,
    digit,
    (<|>),
  )
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import qualified Text.ParserCombinators.Parsec.Combinator as Tok
import qualified Control.Exception as Tok

abc =  Tok.angles lexer identifier

jimpleLocalName :: Parser Name
jimpleLocalName = 
  try identifier
  <|> try abc

jimpleJustExtendClause :: Parser ExtendsClause
jimpleJustExtendClause = do
  reserved "extends"
  name <- jimpleClassName
  Tok.whiteSpace lexer
  return $ Just name

jimpleExtendClause :: Parser ExtendsClause
jimpleExtendClause =
  try jimpleJustExtendClause
    <|> return Nothing

jimpleJustThrowsClause :: Parser ExtendsClause
jimpleJustThrowsClause = do
  reserved "throws"
  Just <$> jimpleClassName

jimpleThrowsClause :: Parser ExtendsClause
jimpleThrowsClause =
  try jimpleJustThrowsClause
    <|> return Nothing

jimpleComment :: Parser String
jimpleComment = do
  string "/*"
  contents <- manyTill anyChar (try (string "*/"))
  Tok.whiteSpace lexer
  return $ contents

jimpleClassComment :: Parser [String]
jimpleClassComment = try $ many jimpleComment



jimpleClassMemberField :: Parser ClassMember
jimpleClassMemberField = do
  modifier <- many jimpleModifier
  type_ <- jimpleType
  name <- identifier
  reservedOp ";"
  return $ ClassField modifier type_ name


jimpleMethodBodyEmpty :: Parser MethodBody
jimpleMethodBodyEmpty = do
  reservedOp ";"
  return EmptyMethod
jimpleDeclaration :: Parser MethodBodyField
jimpleDeclaration = do
  type_ <- jimpleType
  variables <- Tok.commaSep1 lexer jimpleLocalName
  reservedOp ";"
  return $ Declaration type_ variables

jimpleStatementTypedIdentity :: Parser Statement
jimpleStatementTypedIdentity = do
  localName <- jimpleLocalName
  reservedOp ":="
  name <- atIdentifier
  type_ <- jimpleType
  reservedOp ";"
  return $ Identity localName name type_

jimpleStatementNonTypedIdentity :: Parser Statement
jimpleStatementNonTypedIdentity = do
  localName <- jimpleLocalName
  reservedOp ":="
  name <- atIdentifier
  reservedOp ";"
  return $ Identity localName name Void


jimpleStatementIdentity :: Parser Statement
jimpleStatementIdentity =
  try jimpleStatementTypedIdentity
  <|> try jimpleStatementNonTypedIdentity

jimpleStatementThrow :: Parser Statement
jimpleStatementThrow = do
  reserved "throw"
  what <- jimpleImmediate
  reservedOp ";"
  return $ Throw what


jimpleStatementReturn :: Parser Statement
jimpleStatementReturn = do
  reserved "return"
  name <- Just <$> jimpleImmediate <|> return Nothing
  reservedOp ";"
  return $ Return name

jimpleStatementIfGoto :: Parser Statement
jimpleStatementIfGoto = do
  reserved "if"
  cond <- jimpleBoolExpr
  reserved "goto"
  label <- identifier
  reservedOp ";"
  return $ IfGoto cond label

jimpleStatementLocation :: Parser Statement
jimpleStatementLocation = do
  string "/*"
  Tok.whiteSpace lexer
  value <- many1 digit
  Tok.whiteSpace lexer
  string "*/"
  Tok.whiteSpace lexer
  return $ Location value


jimpleStatementGoto :: Parser Statement
jimpleStatementGoto = do
  reserved "goto"
  label <- identifier
  reservedOp ";"
  return $ Goto label

jimpleStatementLabel :: Parser Statement
jimpleStatementLabel = do
  label <- identifier
  reservedOp ":"
  return $ Label label


jimpleStatementInvoke :: Parser Statement
jimpleStatementInvoke = do
  statement <- jimpleInvokeExpr
  reservedOp ";"
  return $ Invoke statement

jimpleStatementAssignment :: Parser Statement
jimpleStatementAssignment = do
  var <- jimpleVariable
  reservedOp "="
  expression <- jimpleExpression
  reservedOp ";"
  return $ Assignement var expression

jimpleStatementCatch :: Parser Statement
jimpleStatementCatch = do
  reserved "catch"
  className <- jimpleClassName
  reserved "from"
  from <- identifier
  reserved "to"
  to <- identifier
  reserved "with"
  with <- identifier
  reservedOp ";"
  return $ Catch className from to with

jimpleCaseStatementDefault :: Parser CaseStatement
jimpleCaseStatementDefault = do
  reserved "default:"
  reserved "goto"
  label <- identifier
  reservedOp ";"
  return $ Default label

jimpleCaseStatementCorner :: Parser CaseStatement
jimpleCaseStatementCorner = do
  reserved "case"
  value <- jimpleImmediate
  reservedOp ":"
  reserved "goto"
  label <- identifier
  reservedOp ";"
  return $ Case value label

jimpleCaseStatement :: Parser CaseStatement
jimpleCaseStatement =
  try jimpleCaseStatementDefault
  <|> try jimpleCaseStatementCorner

jimpleStatementSwitch :: Parser Statement
jimpleStatementSwitch = do
  reserved "lookupswitch"
  condition <- Tok.parens lexer $ jimpleImmediate
  jumps <- Tok.braces lexer $ many jimpleCaseStatement
  reservedOp ";"
  return $ LookupSwitch condition jumps


--class HelloWorld extends java.lang.Object {void <init>() { if $z0 != 0 goto label1; }  }
jimpleStatement :: Parser Statement
jimpleStatement =
  try jimpleStatementInvoke
    <|> try jimpleStatementThrow
    <|> try jimpleStatementIdentity
    <|> try jimpleStatementReturn
    <|> try jimpleStatementAssignment
    <|> try jimpleStatementIfGoto
    <|> try jimpleStatementGoto
    <|> try jimpleStatementLabel
    <|> try jimpleStatementLocation
    <|> try jimpleStatementCatch
    <|> try jimpleStatementSwitch
    -- <|> try jimpleStatementAssignmentDeref

jimpleMethodFullBodyStmt :: Parser MethodBodyField
jimpleMethodFullBodyStmt = do
  Statement <$> jimpleStatement

jimpleMethodBodyField :: Parser MethodBodyField
jimpleMethodBodyField =
  try jimpleMethodFullBodyStmt
    <|> try jimpleDeclaration

jimpleParseFullBody :: Parser MethodBody
jimpleParseFullBody = do
  declarations <- many jimpleMethodBodyField
  return $ FullBody declarations

jimpleMethodFullBody :: Parser MethodBody
jimpleMethodFullBody = do
  Tok.braces lexer jimpleParseFullBody

jimpleMethodBody :: Parser MethodBody
jimpleMethodBody =
  try jimpleMethodBodyEmpty
    <|> try jimpleMethodFullBody

jimpleClassMemberMethod :: Parser ClassMember
jimpleClassMemberMethod = do
  comment <- jimpleClassComment
  modifier <- many jimpleModifier
  type_ <- jimpleType
  name <- identifier
  parameters <- jimpleParameter
  throws <- jimpleThrowsClause
  ClassMethod modifier type_ name parameters throws comment <$> jimpleMethodBody

test = ClassComment  <$> jimpleComment

jimpleClassMember :: Parser ClassMember
jimpleClassMember =  
    try jimpleClassMemberField
    <|> try jimpleClassMemberMethod
    <|> ClassComment  <$> jimpleComment

jimpleFileBody :: Parser FileBody
jimpleFileBody = do
  Tok.braces lexer $ many jimpleClassMember

jimpleFile :: Parser JimpleFile
jimpleFile = do
  modifier <- many jimpleModifier
  reserved "class"
  name <- jimpleClassName
  extends <- jimpleExtendClause
  JimpleFile modifier Class name extends Nothing <$> jimpleFileBody

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseTopLevel :: String -> Either ParseError JimpleFile
parseTopLevel = parse (contents jimpleFile) "<error>"

-- This function is for test purposes
unitParseTest :: Parser a -> String -> Either ParseError a
unitParseTest f = parse (contents f) "<error>"
