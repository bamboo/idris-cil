{-# LANGUAGE OverloadedStrings #-}
module IRTS.Cil.FFI
  ( parseDescriptor
  , parseForeignFunctionType
  , assemblyNameAndTypeFrom
  , foreignType
  , isIO
  , CILForeign(..)
  , ForeignFunctionType(..)
  ) where

import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Text hiding (map, init, last)

import           IRTS.Lang (FDesc(..))
import           Idris.Core.TT (Name(..), sUN)

import           Language.Cil (PrimitiveType(..))
import           Language.Cil.Pretty (pr)

type CILTy = PrimitiveType

data CILForeign
  = CILInstance       !String
  | CILInstanceCustom !String ![CILTy] !CILTy
  | CILInstanceField  !String
  | CILStatic         !CILTy !String
  | CILStaticField    !CILTy !String
  | CILConstructor
  | CILTypeOf         !CILTy
  | CILDelegate       !CILTy
  | CILEnumValueOf    !CILTy !Integer
  | CILExport         !String
  | CILDefault
  deriving Show

parseDescriptor :: FDesc -> CILForeign
parseDescriptor (FApp ffi [declType, FStr fn])
  | ffi == sUN "CILStatic"         = CILStatic (foreignType declType) fn
parseDescriptor (FApp ffi [declType, FStr fn])
  | ffi == sUN "CILStaticField"    = CILStaticField (foreignType declType) fn
parseDescriptor (FApp ffi [FStr fn])
  | ffi == sUN "CILInstance"       = CILInstance fn
parseDescriptor (FApp ffi [FStr fn, paramTys, retTy])
  | ffi == sUN "CILInstanceCustom" = CILInstanceCustom fn (foreignTypeList paramTys) (foreignType retTy)
parseDescriptor (FApp ffi [FStr fn])
  | ffi == sUN "CILInstanceField"  = CILInstanceField fn
parseDescriptor (FCon ffi)
  | ffi == sUN "CILConstructor"    = CILConstructor
parseDescriptor (FApp ffi [ty])
  | ffi == sUN "CILDelegate"       = CILDelegate (foreignType ty)
parseDescriptor (FApp ffi [ty])
  | ffi == sUN "CILTypeOf"         = CILTypeOf (foreignType ty)
parseDescriptor (FApp ffi [ty, FStr i])
  | ffi == sUN "CILEnumValueOf"    = CILEnumValueOf (foreignType ty) (read i)
parseDescriptor e = error $ "invalid foreign descriptor: " <> show e

isIO :: FDesc -> Bool
isIO (FIO _) = True
isIO _       = False

foreignType :: FDesc -> PrimitiveType
foreignType (FStr exportedDataType) = ValueType "" exportedDataType
foreignType (FCon t)                = foreignTypeByName t
foreignType (FIO t)                 = foreignType t

foreignType (FApp cilTy [_, ty])
  | cilTy == sUN "CIL_Array" = foreignType ty

foreignType (FApp cilTy [ty])
  | cilTy == sUN "CIL_CILT" = foreignType ty

foreignType (FApp cilTy [_, ty, _])
  | cilTy == sUN "CIL_FnT" = foreignType ty

foreignType (FApp cilTy [ty])
  | cilTy == sUN "CILTyArr" = Array $ foreignType ty

foreignType (FApp cilTy [FStr assembly, FStr typeName])
  | cilTy == sUN "CILTyRef" =
    case (assembly, typeName) of
      ("", "object") -> Object
      ("", "string") -> String
      _              -> ReferenceType assembly typeName

foreignType (FApp cilTy [FStr assembly, FStr typeName])
  | cilTy == sUN "CILTyVal" =
    case (assembly, typeName) of
      ("", "float32") -> Float32
      ("", "float64") -> Double64
      ("", "bool")    -> Bool
      ("", "int")     -> Int32
      ("", "char")    -> Char
      _               -> ValueType assembly typeName

foreignType (FApp cilTy [_, FCon (UN cilIntTy)])
  | cilTy == sUN "CIL_IntT" =
    let intName = unpack cilIntTy
    in case intName of
         "CIL_IntChar"   -> Char
         "CIL_IntNative" -> Int32
         _               -> error $ "Unsupported foreign int type `" <> intName <> "'"

foreignType (FApp cilTy [def, typeArgs])
  | cilTy == sUN "CILTyGen" = let (ReferenceType assembly typeName) = cilTyDef
                              in GenericReferenceTypeInstance assembly typeName cilTyArgs
  where cilTyDef  = foreignType def
        cilTyArgs = foreignTypeList typeArgs

foreignType (FApp cilTy [FStr paramIndex])
  | cilTy == sUN "CILTyGenParam" = GenericType (read paramIndex)

foreignType d = error $ "invalid type descriptor: " <> show d

foreignTypeList :: FDesc -> [CILTy]
foreignTypeList = (foreignType <$>) . foreignList

foreignList :: FDesc -> [FDesc]
foreignList (FApp tag [_, x, xs])
  | tag == sUN "::"  = x : foreignList xs
foreignList (FApp tag [_])
  | tag == sUN "Nil" = []
foreignList d        = error $ "invalid foreign list: " <> show d

assemblyNameAndTypeFrom :: PrimitiveType -> (String, String)
assemblyNameAndTypeFrom (ReferenceType assemblyName typeName) = (assemblyName, typeName)
assemblyNameAndTypeFrom (ValueType     assemblyName typeName) = (assemblyName, typeName)
assemblyNameAndTypeFrom gti@GenericReferenceTypeInstance{}    = ("", pr gti "")
assemblyNameAndTypeFrom String = ("", "string")
assemblyNameAndTypeFrom Object = ("", "object")
assemblyNameAndTypeFrom Int32  = ("", "int32")
assemblyNameAndTypeFrom Double64 = ("", "float64")
assemblyNameAndTypeFrom t = error $ "unsupported assembly name for: " <> show t

foreignTypeByName :: Name -> PrimitiveType
foreignTypeByName (UN typeName) =
  fromMaybe
    (unsupportedForeignType $ unpack typeName)
    (HM.lookup typeName foreignTypes)
foreignTypeByName n = unsupportedForeignType $ show n

unsupportedForeignType :: String -> a
unsupportedForeignType = error . ("Unsupported foreign type: " <>)

foreignTypes :: HM.HashMap Text PrimitiveType
foreignTypes = HM.fromList [ ("CIL_Str",   String)
                           , ("CIL_Ptr",   Object)
                           , ("CIL_Float", Double64)
                           , ("CIL_Bool",  Bool)
                           , ("CIL_Unit",  Void)
                           ]

data ForeignFunctionType
  = ForeignFunctionType { parameterTypes :: ![PrimitiveType]
                        , returnType     :: !PrimitiveType
                        , returnTypeIO   :: !Bool }
  deriving (Eq, Ord, Show)

data CILFn
  = CILFnIO !PrimitiveType
  | CILFn   !PrimitiveType

unCILFn :: CILFn -> PrimitiveType
unCILFn (CILFnIO t) = t
unCILFn (CILFn t)   = t

parseForeignFunctionType :: FDesc -> ForeignFunctionType
parseForeignFunctionType (FApp n [_, _, fnT]) | n == sUN "CIL_FnT" =
  let ft = functionType fnT
      retType = last ft
      io = case retType of { CILFnIO _ -> True; _ -> False }
  in ForeignFunctionType (unCILFn <$> init ft) (unCILFn retType) io
parseForeignFunctionType d = functionTypeError d

functionType :: FDesc -> [CILFn]
functionType (FApp n [_, _, pT, fnT]) | n == sUN "CIL_Fn"     = CILFn (foreignType pT) : functionType fnT
functionType (FApp n [_, _, ret])     | n == sUN "CIL_FnIO"   = [CILFnIO (foreignType ret)]
functionType (FApp n [_, ret])        | n == sUN "CIL_FnBase" = [CILFn (foreignType ret)]
functionType d = functionTypeError d

functionTypeError :: FDesc -> a
functionTypeError d = error $ "foreign function signature: " <> show d
