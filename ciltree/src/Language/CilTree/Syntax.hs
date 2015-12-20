module Language.CilTree.Syntax
       ( Exp(..)
       , BinOp(..)
       , Local(..)
       , LocalId
       , ExpType(..)
       , ConstValue(..)
       , PrimitiveType(..)
       , MethodRef(..)
       , MemberKind(..)
       , MethodDef(..)
       , FieldRef(..)
       ) where

import Language.Cil (PrimitiveType(..))

data Exp = Let      Local Exp Exp
         | Get      Local
         | GetAddr  Local
         | GetArg   Int
         | GetField Exp FieldRef
         | Call     Bool MethodRef [Exp] -- Call TailCall Method Arguments
         | New      PrimitiveType [PrimitiveType] [Exp]
         | Unary    PrimitiveType UnOp  Exp
         | Binary   PrimitiveType BinOp Exp Exp
         | Seq      [Exp] -- executes all expressions, returns the value of the last one
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
                deriving Show

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

data MethodDef = MethodDef { methodSignature :: MethodRef
                           , methodBody      :: Exp }
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

example1 :: Exp
example1 =
  let var = Local (TypeHole 1) 1
  in Call False
          (MethodRef Instance String "Substring" (Type String) [Type Int32])
          [Get var, Const $ CInt32 1]


example2 :: Exp
example2 =
  let result = Local (Type Int32) 1
  in Let result (Const $ CInt32 0) $
       Seq [ Call False
                  (MethodRef Static Int32 "TryParse" (Type Bool) [Type String, Type (ByRef Int32)])
                  [GetArg 0, GetAddr result]
           , Get result ]
