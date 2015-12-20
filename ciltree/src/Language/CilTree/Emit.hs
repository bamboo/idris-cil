{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}

module Language.CilTree.Emit where

import           Control.Monad.Writer.Strict
import           Data.DList (DList, toList)
import qualified Language.Cil as Cil
import           Language.CilTree.Syntax


cilFor :: MethodDef -> String
cilFor m = Cil.pr (emitMethod m) ""


emitMethod :: MethodDef -> Cil.MethodDef
emitMethod MethodDef{..} = cilMethodFor methodSignature (toList body)
  where body = execWriter (emitMethodBody methodBody)


cilMethodFor :: MethodRef -> [Cil.MethodDecl] -> Cil.MethodDef
cilMethodFor MethodRef{..} = Cil.Method attrs (typeFor returnType) methodName parameters
  where attrs = [Cil.MaStatic, Cil.MaAssembly]
        parameters = zipWith param [0..] parameterTypes
        param i t = Cil.Param Nothing (typeFor t) ("p" ++ show i)


typeFor :: ExpType -> Cil.PrimitiveType
typeFor (Type t) = t
typeFor _        = Cil.Object


type CIL = Writer (DList Cil.MethodDecl)


emitMethodBody :: Exp -> CIL ()
emitMethodBody body = do
  emit body
  tell [ Cil.ret ]


emit :: Exp -> CIL ()
emit (Let local value body) = do
  declare local
  emit value
  store local
  emit body

emit (Const c) = emitConst c

emit (Seq es) = emitSeq es
  where emitSeq []      = pure ()
        emitSeq [e]     = emit e
        emitSeq (e:es') = do emit e
                             tell [ Cil.pop ]
                             emitSeq es'

emit (Call _tailCall MethodRef{..} args) = do
  mapM_ emit args
  let (assemblyName, typeName) = assemblyNameAndTypeFrom methodOwner
  tell [ Cil.call [] retType assemblyName typeName methodName paramTypes ]
  where retType    = typeFor returnType
        paramTypes = map typeFor parameterTypes

emit (If c t e) = do
  let thenLabel = "L1"
  let endLabel  = "L2"
  emit c
  tell [ Cil.brtrue thenLabel ]
  emit e
  tell [ Cil.br endLabel
       , Cil.label thenLabel ]
  emit t
  tell [ Cil.label endLabel ]

emit (Binary Object Eql x y) = do
  emit x
  emit y
  tell [ Cil.ceq ]

emit (GetField obj FieldRef{..}) = do
  emit obj
  let (assemblyName, typeName) = assemblyNameAndTypeFrom fieldOwner
  tell [ Cil.ldfld (typeFor fieldType) assemblyName typeName fieldName ]

emit (Get local) = tell [ Cil.ldlocN (localName local) ]
emit (GetAddr local) = tell [ Cil.ldlocaN (localName local) ]
emit (GetArg i) = tell [ Cil.ldarg i ]
emit Null = tell [ Cil.ldnull ]
emit e = error $ "Unsupported expression: " ++ show e


emitConst :: ConstValue -> CIL ()
emitConst (CInt32 i) = tell [ Cil.ldc_i4 (fromIntegral i) ]
emitConst (CStr s)   = tell [ Cil.ldstr s ]
emitConst c          = error $ "Unsupported const: " ++ show c


declare :: Local -> CIL ()
declare local@(Local t _) = tell [ Cil.localsInit [ Cil.Local (typeFor t) (localName local) ] ]


store :: Local -> CIL ()
store local = tell [ Cil.stlocN (localName local) ]


localName :: Local -> String
localName (Local _ i) = "l" ++ show i


assemblyNameAndTypeFrom :: PrimitiveType -> (String, String)
assemblyNameAndTypeFrom (ReferenceType assemblyName typeName) = (assemblyName, typeName)
assemblyNameAndTypeFrom (ValueType     assemblyName typeName) = (assemblyName, typeName)
assemblyNameAndTypeFrom String = ("", "string")
assemblyNameAndTypeFrom Object = ("", "object")
assemblyNameAndTypeFrom Int32  = ("", "int")
assemblyNameAndTypeFrom t = error $ "unsupported assembly name for: " ++ show t
