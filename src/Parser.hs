module Parser where

import Ast
import Lexer
  ( atIdentifier,
    dotSep,
    fullIdentifier,
    identifier,
    lexer,
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

jimpleIdentifier :: Parser ClassName
jimpleIdentifier = Identifier <$> identifier

jimpleFullIdentifier :: Parser ClassName
jimpleFullIdentifier = FullIdentifier <$> fullIdentifier

jimpleClassName :: Parser ClassName
jimpleClassName =
  try jimpleFullIdentifier
    <|> try jimpleIdentifier

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

jimpleClassMemberField :: Parser ClassMember
jimpleClassMemberField = do
  modifier <- many jimpleModifier
  type_ <- jimpleType
  name <- identifier
  reservedOp ";"
  return $ ClassField modifier type_ name

jimpleParameter :: Parser ParameterList
jimpleParameter = do
  Tok.parens lexer $ Tok.commaSep lexer jimpleType

hackParameter = do
  Tok.parens lexer $ many $ noneOf [')']
  return []

jimpleMethodBodyEmpty :: Parser MethodBody
jimpleMethodBodyEmpty = do
  reservedOp ";"
  return EmptyMethod

jimpleJimpleTypeUnknown :: Parser JimpleType
jimpleJimpleTypeUnknown = do
  reserved "unknown"
  return Unknown

jimpleJimpleTypeNonVoid :: Parser JimpleType
jimpleJimpleTypeNonVoid = do
  NonVoidType <$> jimpleTypeNonVoidBaseFull

jimpleLocalName :: Parser LocalName
jimpleLocalName = do
  identifier

jimpleJimpleType :: Parser JimpleType
jimpleJimpleType =
  try jimpleJimpleTypeUnknown
    <|> jimpleJimpleTypeNonVoid

jimpleDeclaration :: Parser MethodBodyField
jimpleDeclaration = do
  type_ <- jimpleJimpleType
  variables <- Tok.commaSep1 lexer jimpleLocalName
  reservedOp ";"
  return $ Declaration type_ variables

jimpleStatementIdentity :: Parser Statement
jimpleStatementIdentity = do
  localName <- jimpleLocalName
  reservedOp ":="
  name <- atIdentifier
  type_ <- jimpleType
  reservedOp ";"
  return $ Identity localName name type_

jimpleMethodSignatureBase :: Parser MethodSignature
jimpleMethodSignatureBase = do
  char '<'
  name <- jimpleClassName
  reservedOp ":"
  type_ <- jimpleType
  method_name <- identifier
  parameters <- Tok.parens lexer $ many $ noneOf ")"
  char '>'
  return $ MethodSignature name type_ method_name [parameters]

jimpleMethodSignature :: Parser MethodSignature
jimpleMethodSignature = do
  methodSignature <- jimpleMethodSignatureBase
  parameters2 <- Tok.parens lexer $ many $ noneOf ")"
  return methodSignature

-- Helper ADT
data JimpleNamedSignature
  = JimpleNamedSignature Name
  | JimpleMethodSignature MethodSignature

extractName :: JimpleNamedSignature -> Name
extractName (JimpleNamedSignature x) = x

extractSignature :: JimpleNamedSignature -> MethodSignature
extractSignature (JimpleMethodSignature y) = y

toJimpleNamed =
  try (JimpleMethodSignature <$> jimpleMethodSignature)
    <|> try (JimpleNamedSignature <$> identifier)

jimpleStatementVirtualInvoke :: Parser InvokeExpr
jimpleStatementVirtualInvoke = do
  reserved "virtualinvoke"
  namedsignature <- dotSep toJimpleNamed
  return $ VirtualInvoke (extractName $ head namedsignature) (extractSignature $ last namedsignature) []

jimpleStatementSpecialInvoke :: Parser InvokeExpr
jimpleStatementSpecialInvoke = do
  reserved "specialinvoke"
  namedsignature <- dotSep toJimpleNamed
  return $ StaticInvoke (extractName $ head namedsignature) (extractSignature $ last namedsignature) []

jimpleStatementStaticInvoke :: Parser InvokeExpr
jimpleStatementStaticInvoke = do
  reserved "staticinvoke"
  reservedOp "<"
  baseclass <- jimpleClassName
  reservedOp ":"
  castType <- jimpleType
  name <- identifier
  functionParameters <- hackParameter
  reservedOp ">"
  parameters <- hackParameter
  return $ StaticInvoke "" (MethodSignature baseclass castType name []) []


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

jimpleImmediateValue = do
  value <- many1 digit
  return $ Value value

jimpleImmediate =
  try (Local <$> identifier)
    <|> try jimpleImmediateValue

jimpleBinOp = try (reservedOp "==" >> return CmpEq)

jimpleBoolExpr :: Parser Expression
jimpleBoolExpr = do
  lhs <- jimpleImmediate
  op <- jimpleBinOp
  rhs <- jimpleImmediate
  Tok.whiteSpace lexer
  return $ BinOp lhs rhs op

jimpleStatementGoto :: Parser Statement
jimpleStatementGoto = do
  reserved "goto"
  label <- identifier
  reservedOp ";"
  return $ Goto label

jimpleStatementLabel :: Parser Statement
jimpleStatementLabel = do
  label <- many $ noneOf ":"
  reservedOp ":"
  return $ Label label

jimpleInvokeExpr :: Parser InvokeExpr
jimpleInvokeExpr =
    try jimpleStatementStaticInvoke
    <|> try jimpleStatementSpecialInvoke
    <|> try jimpleStatementVirtualInvoke

jimpleStatementInvoke :: Parser Statement
jimpleStatementInvoke = do
  statement <- jimpleInvokeExpr
  reservedOp ";"
  return $ Invoke statement

jimpleStatementAssignment :: Parser Statement
jimpleStatementAssignment = do
  var <- identifier
  reservedOp "="
  expression <- jimpleExpression
  reservedOp ";"
  return $ Assignement var expression

--class HelloWorld extends java.lang.Object {void <init>() { if $z0 != 0 goto label1; }  }
jimpleStatement :: Parser Statement
jimpleStatement =
  try jimpleStatementInvoke
    <|> try jimpleStatementIdentity
    <|> try jimpleStatementReturn
    <|> try jimpleStatementAssignment
    <|> try jimpleStatementIfGoto
    <|> try jimpleStatementGoto
    <|> try jimpleStatementLabel

jimpleMethodFullBodyStmt :: Parser MethodBodyField
jimpleMethodFullBodyStmt = do
  Statement <$> jimpleStatement

jimpleMethodBodyField :: Parser MethodBodyField
jimpleMethodBodyField =
  try jimpleDeclaration
    <|> try jimpleMethodFullBodyStmt

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
  modifier <- many jimpleModifier
  type_ <- jimpleType
  name <- identifier
  parameters <- jimpleParameter
  throws <- jimpleThrowsClause
  ClassMethod modifier type_ name parameters throws <$> jimpleMethodBody

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

jimpleExpression :: Parser Expression
jimpleExpression =
  try jimpleFieldAccessExpression
    <|> try (New <$> jimpleNew)
    <|> try (InvokeExpr <$> jimpleInvokeExpr)

jimpleNew :: Parser New
jimpleNew = do
  reserved "new"
  Simple <$> jimpleType

jimpleClassMember :: Parser ClassMember
jimpleClassMember =
  try jimpleClassMemberField
    <|> try jimpleClassMemberMethod

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