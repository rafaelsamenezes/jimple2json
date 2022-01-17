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

instance ToJSON ClassMember where
  toJSON (ClassField modifiers type_ name) = "FIELD"
  toJSON (ClassMethod modifiers type_ name parameters throws body) = classMethod modifiers type_ name parameters throws body

instance ToJSON New where
  toJSON (Simple t) = toJSON t
  toJSON (Multi t i) = toJSON t

instance ToJSON Expression where
  toJSON (New n) = object [("expr_type", "new"), "new" .= toJSON n]
  toJSON (BinOp lhs rhs op) = object [("expr_type", "binop"), "lhs" .= lhs, "rhs" .= rhs, "operator" .= op]
  toJSON (Cast t i) = object [("expr_type", "cast"), "from" .= i, "to" .= t]
  toJSON (FieldAccess classname field t) = object [("expr_type", "field_access"), "from" .= classname, "field" .= field, "type" .= t]
  toJSON x = "expression"

instance ToJSON InvokeExpr where
  toJSON (StaticInvoke name method arguments) = object [("object", "StaticInvoke"), "base_class" .= extractClassName method, "parameters" .= arguments, "method" .= extractMethod method]
  toJSON x = toJSON $ T.pack $ show x

instance ToJSON Statement where
  toJSON (Label x) = object [("object", "Label"), "label_id" .= x, "content" .= ([] :: [Statement])]
  toJSON (LabelDef l) = object [("object", "Label"), "label_id" .= l, "content" .= ([] :: [Statement])]
  toJSON Breakpoint = object [("stmt", "breakpoint")]
  toJSON (Identity n at t) = toJSON $ convertIdentity (Identity n at t)
  toJSON (Return x) = object $ ("object", "Return") : maybeToPair x "value"
  toJSON (Goto l) = object [("stmt", "goto"), "goto" .= l]
  toJSON (Assignement x y) = object [("object", "SetVariable"), "name" .= x, "value" .= y]
  toJSON (IfGoto e l) = object [("object", "If"), "expression" .= e, "goto" .= l]
  toJSON (Invoke x) = toJSON x

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
  toJSON x = toJSON $ show x

instance ToJSON BaseType where
  toJSON (ClassName name) = toJSON name
  toJSON x = toJSON $ T.toLower $ T.pack $ show x

instance ToJSON NonVoidType where
  toJSON (BaseType name size) = object ["typename" .= name, "dimensions" .= size]
  toJSON (TypeIdentifier name size) = object ["typename" .= name, "dimensions" .= size]

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
