{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
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
import           Idris.Core.TT (Name(..))
import           Language.Cil (PrimitiveType(..))

type CILTy = PrimitiveType

data CILForeign = CILInstance    String
                | CILStatic      CILTy String
                | CILStaticField CILTy String
                | CILConstructor
                | CILExport      String
                | CILDefault
                | CILNull
                | CILTypeOf      CILTy
                deriving Show

parseDescriptor :: FDesc -> CILForeign
parseDescriptor (FApp (UN (unpack -> "CILStatic")) [declType, FStr fn]) =
  CILStatic (foreignTypeToCilType declType) fn
parseDescriptor (FApp (UN (unpack -> "CILStaticField")) [declType, FStr fn]) =
  CILStaticField (foreignTypeToCilType declType) fn
parseDescriptor (FApp (UN (unpack -> "CILInstance")) [FStr fn]) =
  CILInstance fn
parseDescriptor (FCon (UN (unpack -> "CILConstructor"))) =
  CILConstructor
parseDescriptor (FCon (UN (unpack -> "CILNull"))) =
  CILNull
parseDescriptor (FApp (UN (unpack -> "CILTypeOf")) [ty]) =
  CILTypeOf (foreignTypeToCilType ty)

parseDescriptor e = error $ "invalid foreign descriptor: " ++ show e

isIO :: FDesc -> Bool
isIO (FIO _) = True
isIO _       = False

foreignTypeToCilType :: FDesc -> PrimitiveType
foreignTypeToCilType (FApp (UN (unpack -> "CIL_CILT")) [ty]) = foreignTypeToCilType ty
foreignTypeToCilType (FApp (UN (unpack -> "CILTyArr")) [ty]) = Array $ foreignTypeToCilType ty
foreignTypeToCilType (FApp (UN (unpack -> "CILTyRef"))
                       [FStr assembly, FStr typeName]) = ReferenceType assembly typeName
foreignTypeToCilType (FApp (UN (unpack -> "CILTyVal"))
                       [FStr assembly, FStr typeName]) = ValueType assembly typeName
foreignTypeToCilType (FApp (UN (unpack -> "CIL_IntT")) _) = Int32
foreignTypeToCilType (FCon t)   = foreignType t
foreignTypeToCilType (FIO t)    = foreignTypeToCilType t
foreignTypeToCilType d          = error $ "invalid type descriptor: " ++ show d

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
