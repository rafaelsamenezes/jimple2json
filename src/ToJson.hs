 {-# LANGUAGE OverloadedStrings #-}

module ToJson where

import Ast
import Data.Aeson


convertMaybe :: ToJSON a => Maybe a -> Value
convertMaybe = maybe Null toJSON

instance ToJSON JimpleFile where
    toJSON (JimpleFile modifier filetype className extendsClause implementsClause fileBody) = object ["modifiers" .= modifier
        , "filetype" .= filetype, "classname" .= className
        , "extends" .= convertMaybe extendsClause
        , "implements" .= convertMaybe implementsClause
        , "filebody" .= fileBody]


classMethod modifiers type_ name parameters throws body = object ["modifiers" .= modifiers
    , "type" .= type_
    , "parameters" .= parameters
    , "throws" .= throws
    , "body" .= body
    , "name" .= name]

instance ToJSON Immediate where
    toJSON (Local x) = toJSON x
    toJSON (Value x) = toJSON x


instance ToJSON ClassMember where
    toJSON (ClassField modifiers type_ name) = "FIELD"
    toJSON (ClassMethod modifiers type_ name parameters throws body) = object ["method" .= classMethod modifiers type_ name parameters throws body]

instance ToJSON New where
    toJSON (Simple t) = toJSON t
    toJSON (Multi t i) = toJSON t

instance ToJSON Expression where
    toJSON (New n) = object [ ("expr_type", "new"), "new" .= toJSON n]
    toJSON (BinOp lhs rhs op) = object [ ("expr_type", "binop"), "binop" .=  object [ "lhs" .= lhs, "rhs" .= rhs, "binop" .= op]]
    toJSON x = "expression"

instance ToJSON Statement where
    toJSON (Label x) = object [ ("stmt", "label"), "label" .= toJSON x ]
    toJSON Breakpoint = object [ ("stmt", "breakpoint") ]
    toJSON (Identity n at t) = object [ ("stmt", "identity"), "identity" .=  object ["name" .= n, "identifier" .= at, "type" .= t]]
    toJSON (Return x) = object [ ("stmt", "return"), "return" .= maybe Null toJSON x ]
    toJSON (Goto l) = object [ ("stmt", "goto"), "goto" .= l ]
    toJSON (LabelDef l) = object [ ("stmt", "label"), "label" .= l ]
    toJSON (Assignement x y) = object [ ("stmt", "assignment"), "assignment" .= object ["name" .= x, "expression" .= y] ]
    toJSON (IfGoto e l) = object [ ("stmt", "ifgoto"), "ifgoto" .= object [ "expr" .= e, "label" .= l] ]
    toJSON (Invoke x) = object [ ("stmt", "invoke") ]

instance ToJSON MethodBodyField where
    toJSON (Statement x) = object [ "statement" .= x]
    toJSON (Declaration t xs) = object [ "declaration" .= object ["type" .= t, "names" .= xs]]

instance ToJSON MethodBody where
    toJSON EmptyMethod = Null
    toJSON (FullBody xs) = toJSON xs

instance ToJSON BinOp where
    toJSON x = toJSON $ show x

instance ToJSON BaseType where
    toJSON x = toJSON $ show x

instance ToJSON NonVoidType where
    toJSON (BaseType name size) = object [ "typename" .= name, "dimensions" .= size]
    toJSON (TypeIdentifier name size) = object [ "typename" .= name, "dimensions" .= size]

instance ToJSON JimpleType where
    toJSON Unknown = "Unknown"
    toJSON (NonVoidType x) = toJSON x

instance ToJSON Type where
    toJSON Void = "void"
    toJSON (NonvoidType x) = toJSON x

instance ToJSON ClassName where
    toJSON (Quoted x) = toJSON x
    toJSON (Identifier x) = toJSON x
    toJSON (FullIdentifier x) = toJSON x

instance ToJSON FileType where
    toJSON x = toJSON $ show x

instance ToJSON Modifier where
    toJSON x = toJSON $ show x

