module Ast where

-- TODO: Properly process identifiers
-- TODO
type Name = String
type LocalName = String
type ParameterList = [Type]
type CatchClause = String
type AtIdentifier = String
type Argument = String
type Label = String

data Immediate = Local Name
               | Value String
               | StringConst String
               deriving (Eq, Ord, Show)

data ClassName = Quoted Name
               | Identifier Name
               | FullIdentifier Name
               deriving (Eq, Ord, Show)

-- TODO: Remove this
data New = Simple Type
         | Multi Type Int -- TODO
         deriving (Eq, Ord, Show)


data Expression = New New
                | NewArray Type Immediate
                | Cast Type Immediate
                | FieldAccess ClassName Name Type
                | InvokeExpr InvokeExpr
                | BinOp Immediate Immediate BinOp
                | Dereference Immediate Immediate
                | Immediate Immediate
                deriving (Eq, Ord, Show)

data BinOp = And
           | Or
           | Xor
           | Mod
           | Cmp
           | CmpG
           | CmpGEq
           | CmpL
           | CmpEq
           | CmpNe
           | Minus
           | Add
           deriving (Eq, Ord, Show)

type BoolExpression = String


data Statement = Label Name
               | Breakpoint
               | Identity LocalName AtIdentifier Type
               | Invoke InvokeExpr
               | Return (Maybe Immediate)
               | Assignement Name Expression
               | AssignementDeref Name Expression Immediate
               | IfGoto Expression Label
               | LabelDef Label
               | Throw Immediate
               | Goto Label
               deriving (Eq, Ord, Show)

convertIdentity :: Statement -> Statement
convertIdentity (Identity var at t) = Assignement var (Cast t $ Local ('@':at))
convertIdentity x = x

data MethodBodyField = Statement Statement
                     | Declaration Type [LocalName]
                     | DeclarationSingle Type LocalName
                     deriving (Eq, Ord, Show)

-- I was having some trouble converting
-- a declaration into multiple json objects
-- So, i created the DeclarationSingle

convertDeclaration :: MethodBodyField -> [MethodBodyField]
convertDeclaration (Declaration t []) = []
convertDeclaration (Declaration t (x:xs)) = DeclarationSingle t x : convertDeclaration (Declaration t xs)
convertDeclaration x = [x]

adaptMethodFields :: [MethodBodyField] -> [MethodBodyField]
adaptMethodFields = concatMap convertDeclaration

data MethodSignature = MethodSignature ClassName Type Name ParameterList
                     deriving (Eq, Ord, Show)

extractClassName :: MethodSignature -> ClassName
extractClassName (MethodSignature a _ _ _) = a

extractMethod :: MethodSignature -> Name
extractMethod (MethodSignature _ _ a _) = a

extractMethodParameters :: MethodSignature -> ParameterList
extractMethodParameters (MethodSignature _ _ _ a) = a

data InvokeExpr = VirtualInvoke Name MethodSignature [Immediate]
                | StaticInvoke MethodSignature [Immediate]
                | SpecialInvoke Name MethodSignature [Immediate]
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
