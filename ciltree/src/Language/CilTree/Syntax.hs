module Language.CilTree.Syntax
       ( AssemblyDefinition(..)
       , TypeDefinition(..)
       , MemberDefinition(..)
       , Exp(..)
       , BinOp(..)
       , Local(..)
       , LocalId
       , ExpType(..)
       , ConstValue(..)
       , PrimitiveType(..)
       , MethodRef(..)
       , MemberKind(..)
       , FieldRef(..)
       ) where


import Language.Cil (PrimitiveType(..))


data AssemblyDefinition = AssemblyDefinition AssemblyName [TypeDefinition]


data TypeDefinition = ClassDefinition String [MemberDefinition]


data MemberDefinition = MethodDefinition { methodSignature :: MethodRef
                                         , methodBody      :: Exp }
                      | ConstructorDefinition MemberKind [ExpType] Exp
                      | FieldDefinition MemberKind String ExpType


type AssemblyName = String


data Exp = Let      Local Exp Exp
         | Get      Local
         | GetAddr  Local
         | GetArg   Int
         | GetField (Maybe Exp) FieldRef
         | Call     Bool (Maybe Exp) MethodRef [Exp] -- Call TailCall Instance Method Arguments
         | New      PrimitiveType [PrimitiveType] [Exp]
         | Unary    PrimitiveType UnOp  Exp
         | Binary   PrimitiveType BinOp Exp Exp
         | Seq      [Exp] -- evaluates all expressions, returns last value
         | If       Exp Exp Exp
         | Switch   Exp [(ConstValue, Exp)]
         | Const    ConstValue
         | Null
         | Bottom
         deriving Show


data Local = Local ExpType LocalId
           deriving Show


type LocalId = Integer


data ExpType = Type     PrimitiveType
             | TypeHole TypeHoleId
             deriving Show


type TypeHoleId = Integer


data ConstValue = CInt32   Int
                | CInt64   Integer
                | CFloat64 Double
                | CChar    Char
                | CStr     String
                | CBool    Bool
                deriving Show


data MemberKind = Static
                | Instance
                deriving (Eq, Ord, Show)


data MethodRef = MethodRef { methodKind     :: MemberKind
                           , methodOwner    :: PrimitiveType
                           , methodName     :: String
                           , returnType     :: ExpType
                           , parameterTypes :: [ExpType] }
               deriving Show


data FieldRef = FieldRef { fieldKind  :: MemberKind
                         , fieldOwner :: PrimitiveType
                         , fieldName  :: String
                         , fieldType  :: ExpType }
              deriving Show


data BinOp = Add
           | Sub
           | Mul
           | Div
           | Eql
           deriving Show


data UnOp = ArrayLength
          | Not
          deriving Show
