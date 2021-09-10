module Ast where

-- TODO: Properly process identifiers
-- TODO
type Name = String
type LocalName = String
type ParameterList = String
type CatchClause = String
type AtIdentifier = String
type Argument = String
type Label = String

data Immediate = Local Name
               | Value String
               deriving (Eq, Ord, Show)

data ClassName = Quoted Name
               | Identifier Name
               | FullIdentifier Name
               deriving (Eq, Ord, Show)

data New = Simple Type
         | Multi Type Int -- TODO
         deriving (Eq, Ord, Show)


data Expression = New New
                | Cast Type Immediate
                | InvokeExpr InvokeExpr
                | BinOp Immediate Immediate BinOp
                | Immediate Immediate
                deriving (Eq, Ord, Show)

data BinOp = And
           | Or
           | Xor
           | Mod
           | Cmp
           | CmpG
           | CmpL
           | CmpEq
           | CmpNe
           deriving (Eq, Ord, Show)

type BoolExpression = String


data Statement = Label Name
               | Breakpoint
               | Identity LocalName AtIdentifier Type
               | Invoke InvokeExpr
               | Return (Maybe Immediate)
               | Assignement Name Expression
               | IfGoto Expression Label
               | LabelDef Label
               | Goto Label
               deriving (Eq, Ord, Show)

data MethodBodyField = Statement Statement
                     | Declaration JimpleType [LocalName]
                     deriving (Eq, Ord, Show)


data MethodSignature = MethodSignature ClassName Type Name [Argument]
                     deriving (Eq, Ord, Show)

data InvokeExpr = VirtualInvoke Name MethodSignature [Argument]
                | StaticInvoke Name MethodSignature [Argument]
                | DynamicInvoke -- TODO
                deriving (Eq, Ord, Show)


data BaseType = Boolean
              | Byte
              | Char
              | Short
              | Int
              | Long
              | Float
              | Double
              | NullType
              | ClassName ClassName
              deriving (Eq, Ord, Show)

-- TODO: Quoted/Ident/FullIdent
data NonVoidType = BaseType BaseType Int
                 | TypeIdentifier Name Int
                 deriving (Eq, Ord, Show)

data Type = Void
          | NonvoidType NonVoidType
          deriving (Eq, Ord, Show)

data JimpleType = Unknown
                | NonVoidType NonVoidType
                deriving (Eq, Ord, Show)


data MethodBody = EmptyMethod
                | FullBody [MethodBodyField]
                deriving (Eq, Ord, Show)

data ClassMember = ClassField [Modifier] Type Name
                 | ClassMethod [Modifier] Type Name ParameterList ThrowsClause MethodBody
                 deriving (Eq, Ord, Show)

type FileBody = [ClassMember]
data Modifier = Abstract
              | Final
              | Native
              | Public
              | Protected
              | Private
              | Static
              | Synchronized
              | Transient
              | Volatile
              | StrictFp
              | Enum
              | Annotation
              deriving (Eq, Ord, Show)


data FileType = Class
              | Interface
              deriving (Eq, Ord, Show)

type ExtendsClause = Maybe ClassName
type ThrowsClause = Maybe ClassName
type ImplementsClause = Maybe ClassName
data JimpleFile = JimpleFile [Modifier] FileType ClassName ExtendsClause ImplementsClause FileBody deriving (Eq, Ord, Show)
