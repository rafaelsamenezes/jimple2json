module Ast where

-- Base Types
type Name = String

type CatchClause = String

type AtIdentifier = String

type Argument = String

type Label = String

-- Type
data BaseType
  = Boolean
  | Byte
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
  | NullType
  | ClassName ClassName
  deriving (Eq, Show)

-- TODO: Quoted/Ident/FullIdent
data NonVoidType
  = BaseType BaseType Int
  | TypeIdentifier Name Int
  deriving (Eq, Show)

data Type
  = Void
  | NonvoidType NonVoidType
  deriving (Eq, Show)

-- Common
type ParameterList = [Type]

data Modifier
  = Abstract
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
  deriving (Eq, Show)

type ThrowsClause = Maybe ClassName

-- Expression
data Expression
  = New New -- new java.util.random
  | NewArray Type Immediate -- newarray (int)[20]
  | Cast Type Immediate -- (int) 1
  | ReferenceExpr Reference -- r0.<Foo: int A>
  | InvokeExpr InvokeExpr -- specialinvoke $r0.<java.util.Random: void <init>()>();
  | BinOp Immediate Immediate BinOp -- 1 + 1
  | UnOp Immediate UnOp -- -1
  | Immediate Immediate -- a
  deriving (Eq, Show)

data Immediate
  = Local Name
  | Value String
  | StringConst String
  | Clzz String -- class reference
  deriving (Eq, Show)

data InvokeExpr
  = VirtualInvoke Name MethodSignature [Immediate]
  | StaticInvoke MethodSignature [Immediate]
  | SpecialInvoke Name MethodSignature [Immediate]
  | DynamicInvoke -- TODO
  deriving (Eq, Show)

data BinOp
  = And
  | Or
  | Xor
  | Mod
  | Cmp
  | Greater
  | Less
  | CmpG
  | CmpGEq
  | CmpL
  | CmpLEq
  | CmpEq
  | CmpNe
  | Minus
  | Add
  | Times
  | Division
  deriving (Eq, Show)

data UnOp
  = LengthOf
  | Neg
  deriving (Eq, Show)

-- TODO: Remove this
data New
  = Simple Type
  | Multi Type Int -- TODO
  deriving (Eq, Show)

-- ??????????????
type BoolExpression = String

-- Statement
data CaseStatement
  = Default Name
  | Case Immediate Name
  deriving (Eq, Show)

data Statement
  = Label Name
  | Breakpoint
  | Identity Name AtIdentifier Type
  | Invoke InvokeExpr
  | Return (Maybe Immediate)
  | Assignement Variable Expression
  | IfGoto Expression Label
  | LabelDef Label
  | Throw Immediate
  | Goto Label
  | Location Name
  | Catch ClassName Name Name Name
  | LookupSwitch Immediate [CaseStatement]
  deriving (Eq, Show)

-- Class Members
data ClassName
  = Quoted Name
  | Identifier Name
  | FullIdentifier Name
  deriving (Eq, Show)

data FieldSignature = FieldSignature ClassName Type Name
  deriving (Eq, Show)

data FieldReference
  = LocalFieldReference Name FieldSignature
  | FieldSignatureRef FieldSignature
  deriving (Eq, Show)

data Reference
  = ArrayRef Name Immediate -- Index of array, e.g. foo[2], Name: foo, Imeediate: 2
  | FieldRef FieldReference -- A full access, e.g. foo.bar()
  deriving (Eq, Show)

data Variable
  = LocalName Name
  | Reference Reference
  deriving (Eq, Show)

-- File

data MethodBodyField
  = Statement Statement
  | Declaration Type [Name]
  | DeclarationSingle Type Name
  deriving (Eq, Show)

data MethodSignature = MethodSignature ClassName Type Name ParameterList
  deriving (Eq, Show)

-- Jimple File
data FileType
  = Class
  | Interface
  deriving (Eq, Show)

type ExtendsClause = Maybe ClassName

type ImplementsClause = Maybe ClassName

type FileBody = [ClassMember]

data MethodBody
  = EmptyMethod
  | FullBody [MethodBodyField]
  deriving (Eq, Show)

data ClassMember
  = ClassField [Modifier] Type Name
  | ClassMethod [Modifier] Type Name ParameterList ThrowsClause [String] MethodBody
  | ClassComment String
  deriving (Eq, Show)

data JimpleFile = JimpleFile [Modifier] FileType ClassName ExtendsClause ImplementsClause FileBody deriving (Eq, Show)
