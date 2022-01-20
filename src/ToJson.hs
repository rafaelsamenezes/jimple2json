{-# LANGUAGE OverloadedStrings #-}

module ToJson where

import Ast
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T

maybeToPair :: ToJSON a => Maybe a -> T.Text -> [Pair]
maybeToPair Nothing _ = []
maybeToPair (Just x) k = [k .= x]

convertMaybe :: ToJSON a => Maybe a -> Value
convertMaybe = maybe Null toJSON

instance ToJSON JimpleFile where
  toJSON (JimpleFile modifier filetype className extendsClause implementsClause fileBody) =
    object $
      [ "object" .= filetype,
        "modifiers" .= modifier,
        "name" .= className,
        "content" .= fileBody
      ]
        ++ maybeToPair extendsClause "extends"
        ++ maybeToPair implementsClause "implements"

classMethod modifiers type_ name parameters throws body =
  object
    [ "object" .= T.pack "Method",
      "modifiers" .= modifiers,
      "type" .= type_,
      "parameters" .= parameters,
      "content" .= body,
      "name" .= name
    ]

instance ToJSON Immediate where
  toJSON (Local x) = object ["expr_type" .= T.pack "symbol", "value" .= T.pack x]
  toJSON (Value x) = object ["expr_type" .= T.pack "constant", "value" .= T.pack x]
  toJSON (StringConst x) = object ["expr_type" .= T.pack "string_constant", "value" .= T.pack x]

instance ToJSON ClassMember where
  toJSON (ClassField modifiers type_ name) = "FIELD"
  toJSON (ClassMethod modifiers type_ name parameters throws body) = classMethod modifiers type_ name parameters throws body

instance ToJSON New where
  toJSON (Simple t) = toJSON t
  toJSON (Multi t i) = toJSON t


customJSON (StaticInvoke method arguments) = object [(("expr_type", "static_invoke")),  "base_class" .= extractClassName method, "parameters" .= arguments, "method" .= extractMethod method]

instance ToJSON Expression where
  toJSON (New n) = object [("expr_type", "new"), "type" .= toJSON n]
  toJSON (NewArray t i) = object [("expr_type", "newarray"), "type" .= toJSON t, "size" .= i]
  toJSON (BinOp lhs rhs op) = object [("expr_type", "binop"), "lhs" .= lhs, "rhs" .= rhs, "operator" .= op]
  toJSON (Cast t i) = object [("expr_type", "cast"), "from" .= i, "to" .= t]
  toJSON (FieldAccess classname field t) = object [("expr_type", "field_access"), "from" .= classname, "field" .= field, "type" .= t]
  toJSON (Immediate i) = toJSON i
  toJSON (Dereference base index) = object [("expr_type", "deref"), "base" .= toJSON base, "index" .= toJSON index ]
  toJSON (InvokeExpr e) = customJSON e
  toJSON x = toJSON $ T.pack $ show x

instance ToJSON InvokeExpr where
  toJSON (StaticInvoke method arguments) = object [("object", "StaticInvoke"), "base_class" .= extractClassName method, "parameters" .= arguments, "method" .= extractMethod method]
  toJSON (SpecialInvoke name method arguments) = object [("object", "SpecialInvoke"), "variable" .= name,  "base_class" .= extractClassName method, "parameter_type" .= extractMethodParameters method, "parameters" .= arguments, "method" .= extractMethod method]
  toJSON x = toJSON $ T.pack $ show x

instance ToJSON Statement where
  toJSON (Label x) = object [("object", "Label"), "label_id" .= x, "content" .= ([] :: [Statement])]
  toJSON (Throw x) = object [("object", "Throw"), "expr" .= x]
  toJSON (LabelDef l) = object [("object", "Label"), "label_id" .= l, "content" .= ([] :: [Statement])]
  toJSON Breakpoint = object [("stmt", "breakpoint")]
  toJSON (Identity n at t) = toJSON $ convertIdentity (Identity n at t)
  toJSON (Return x) = object $ ("object", "Return") : maybeToPair x "value"
  toJSON (Goto l) = object [("object", "Goto"), "goto" .= l]
  toJSON (Assignement x y) = object [("object", "SetVariable"), "name" .= x, "value" .= y]
  toJSON (AssignementDeref name value index) = object [("object", "SetVariableDeref"), "name" .= name, "value" .=  toJSON value, "pos" .= index]
  toJSON (IfGoto e l) = object [("object", "If"), "expression" .= e, "goto" .= l]
  toJSON (Invoke x) = toJSON x
  toJSON x =  toJSON $ T.pack $ show x

generateDeclaration t x = object [("object", "Variable"), "type" .= t, "name" .= x]

instance ToJSON MethodBodyField where
  toJSON (Statement x) = toJSON x
  toJSON (Declaration t xs) = toJSON $ map (generateDeclaration t) xs
  toJSON (DeclarationSingle t x) = generateDeclaration t x

instance ToJSON MethodBody where
  toJSON EmptyMethod = Null
  toJSON (FullBody xs) = toJSON $ adaptMethodFields xs

instance ToJSON BinOp where
  toJSON CmpEq = toJSON $ T.pack "=="
  toJSON CmpNe = toJSON $ T.pack "notequal"
  toJSON Add = toJSON $ T.pack "+"
  toJSON Minus = toJSON $ T.pack "-"
  toJSON CmpGEq = toJSON $ T.pack ">="
  toJSON CmpG = toJSON $ T.pack ">"
  toJSON x = toJSON $ show x

instance ToJSON BaseType where
  toJSON (ClassName name) = toJSON name
  toJSON x = toJSON $ T.toLower $ T.pack $ show x

instance ToJSON NonVoidType where
  toJSON (BaseType name size) = object ["identifier" .= name, "dimensions" .= size]
  toJSON (TypeIdentifier name size) = object ["identifier" .= name, "dimensions" .= size]

instance ToJSON JimpleType where
  toJSON Unknown = "Unknown"
  toJSON (NonVoidType x) = toJSON x

instance ToJSON Type where
  toJSON Void = object ["identifier" .= T.pack "void", "dimensions" .= (0 :: Int)]
  toJSON (NonvoidType x) = toJSON x

instance ToJSON ClassName where
  toJSON (Quoted x) = toJSON x
  toJSON (Identifier x) = toJSON x
  toJSON (FullIdentifier x) = toJSON x

instance ToJSON FileType where
  toJSON x = toJSON $ show x

instance ToJSON Modifier where
  toJSON x = toJSON $ T.toLower $ T.pack $ show x
