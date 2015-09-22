{-# LANGUAGE OverloadedStrings #-}
module IRTS.Cil.FFI
       ( parseDescriptor
       , assemblyNameAndTypeFrom
       , foreignTypeToCilType
       , isIO
       , CILForeign(..)
       ) where

import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.Text

import           IRTS.Lang (FDesc(..))
import           Idris.Core.TT (Name(..), sUN)
import           Language.Cil (PrimitiveType(..))

type CILTy = PrimitiveType

data CILForeign = CILInstance    String
                | CILInstanceField String
                | CILStatic      CILTy String
                | CILStaticField CILTy String
                | CILConstructor
                | CILTypeOf      CILTy
                | CILEnumValueOf CILTy Integer
                | CILExport      String
                | CILDefault
                deriving Show

parseDescriptor :: FDesc -> CILForeign
parseDescriptor (FApp ffi [declType, FStr fn])
  | ffi == sUN "CILStatic"        = CILStatic (foreignTypeToCilType declType) fn
parseDescriptor (FApp ffi [declType, FStr fn])
  | ffi == sUN "CILStaticField"   = CILStaticField (foreignTypeToCilType declType) fn
parseDescriptor (FApp ffi [FStr fn])
  | ffi == sUN "CILInstance"      = CILInstance fn
parseDescriptor (FApp ffi [FStr fn])
  | ffi == sUN "CILInstanceField" = CILInstanceField fn
parseDescriptor (FCon ffi)
  | ffi == sUN "CILConstructor"   = CILConstructor
parseDescriptor (FApp ffi [ty])
  | ffi == sUN "CILTypeOf"        = CILTypeOf (foreignTypeToCilType ty)
parseDescriptor (FApp ffi [ty, FStr i])
  | ffi == sUN "CILEnumValueOf"   = CILEnumValueOf (foreignTypeToCilType ty) (read i)
parseDescriptor e = error $ "invalid foreign descriptor: " ++ show e

isIO :: FDesc -> Bool
isIO (FIO _) = True
isIO _       = False

foreignTypeToCilType :: FDesc -> PrimitiveType
foreignTypeToCilType (FStr exportedDataType) = ValueType "" exportedDataType
foreignTypeToCilType (FCon t)                = foreignType t
foreignTypeToCilType (FIO t)                 = foreignTypeToCilType t
foreignTypeToCilType (FApp cilTy [ty])
  | cilTy == sUN "CIL_CILT" = foreignTypeToCilType ty
foreignTypeToCilType (FApp cilTy [ty])
  | cilTy == sUN "CILTyArr" = Array $ foreignTypeToCilType ty
foreignTypeToCilType (FApp cilTy [FStr assembly, FStr typeName])
  | cilTy == sUN "CILTyRef" = ReferenceType assembly typeName
foreignTypeToCilType (FApp cilTy [FStr assembly, FStr typeName])
  | cilTy == sUN "CILTyVal" = ValueType assembly typeName
foreignTypeToCilType (FApp cilTy _)
  | cilTy == sUN "CIL_IntT" = Int32
foreignTypeToCilType d      = error $ "invalid type descriptor: " ++ show d

assemblyNameAndTypeFrom :: PrimitiveType -> (String, String)
assemblyNameAndTypeFrom (ReferenceType assemblyName typeName) = (assemblyName, typeName)
assemblyNameAndTypeFrom (ValueType     assemblyName typeName) = (assemblyName, typeName)
assemblyNameAndTypeFrom String = ("mscorlib", "System.String")
assemblyNameAndTypeFrom Object = ("mscorlib", "System.Object")
assemblyNameAndTypeFrom t = error $ "unsupported assembly name for: " ++ show t

foreignType :: Name -> PrimitiveType
foreignType (UN typeName) =
  fromMaybe
    (unsupportedForeignType $ unpack typeName)
    (HM.lookup typeName foreignTypes)
foreignType n = unsupportedForeignType $ show n

unsupportedForeignType :: String -> a
unsupportedForeignType = error . ("Unsupported foreign type: " ++)

foreignTypes :: HM.HashMap Text PrimitiveType
foreignTypes = HM.fromList [("CIL_Str",   String)
                           ,("CIL_Ptr",   Object)
                           ,("CIL_Float", Float32)
                           ,("CIL_Bool",  Bool)
                           ,("CIL_Unit",  Void)
                           ]
