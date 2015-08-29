{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module IRTS.Cil.FFI
       ( parseDescriptor
       , assemblyNameAndTypeFrom
       , foreignTypeToCilType
       , CILForeign(..)
       ) where

import           Data.Maybe
import           Data.Text
import qualified Data.HashMap.Strict as HM

import           IRTS.Lang (FDesc(..))
import           Idris.Core.TT (Name(..))
import           Language.Cil (PrimitiveType(..))

-- mimic Idris FFI here?
-- type CILTy  = PrimitiveType
-- type CILSig = [CILTy]
-- data CILForeign = CILInstance CILTy String CILSig CILTy
--                 | CILStatic CILTy String CILSig CILTy
--                 | CILConstructor CILTy CILSig
--                 | CILExport String CILSig CILTy
--                 | CILDefault

data CILForeign = CILInstance | CILStatic | CILConstructor

parseDescriptor :: FDesc -> (CILForeign, PrimitiveType, String, [PrimitiveType], PrimitiveType)
parseDescriptor (FApp (UN (unpack -> "CILStatic")) [declType, FStr fn, sig, retType]) =
  (CILStatic, parseCILTy declType, fn, parseCILSig sig, parseCILTy retType)
parseDescriptor (FApp (UN (unpack -> "CILInstance")) [declType, FStr fn, sig, retType]) =
  (CILInstance, parseCILTy declType, fn, parseCILSig sig, parseCILTy retType)
parseDescriptor (FApp (UN (unpack -> "CILConstructor")) [declType, sig]) =
  let decl = parseCILTy declType
  in (CILConstructor, decl, ".ctor", parseCILSig sig, decl)
parseDescriptor e = error $ "invalid foreign descriptor: " ++ show e

parseCILTy :: FDesc -> PrimitiveType
parseCILTy (FApp (UN (unpack -> "CILTyRef")) [FStr assemblyName, FStr typeName]) =
  ReferenceType assemblyName typeName
parseCILTy (FCon (UN (unpack -> conName))) =
  case conName of
    "CILTyFloat32" -> Float32
    "CILTyInt32"   -> Int32
    "CILTyStr"     -> String
    "CILTyBool"    -> Bool
    "CILTyObj"     -> Object
    "CILTyVoid"    -> Void
    t              -> error $ "unsupported CILTy constructor: " ++ t
parseCILTy d = error $ "unsupported CILTy descriptor: " ++ show d

parseCILSig :: FDesc -> [PrimitiveType]
parseCILSig (FApp (UN (unpack -> "::")) [_, t, ts]) = parseCILTy t : parseCILSig ts
parseCILSig (FApp (UN (unpack -> "Nil")) _) = []
parseCILSig d = error $ "invalid signature: " ++ show d

foreignTypeToCilType :: FDesc -> PrimitiveType
foreignTypeToCilType (FApp (UN (unpack -> "CIL_AnyT")) args) = error $ show args
foreignTypeToCilType (FApp t _) = foreignType t
foreignTypeToCilType (FCon t)   = foreignType t
foreignTypeToCilType (FIO t)    = foreignTypeToCilType t
foreignTypeToCilType d          = error $ "invalid type descriptor: " ++ show d

assemblyNameAndTypeFrom :: PrimitiveType -> (String, String)
assemblyNameAndTypeFrom (ReferenceType assemblyName typeName) = (assemblyName, typeName)
assemblyNameAndTypeFrom String = ("mscorlib", "System.String")
assemblyNameAndTypeFrom Object = ("mscorlib", "System.Object")
assemblyNameAndTypeFrom t = error $ "unsupported assembly name for: " ++ show t

foreignType :: Name -> PrimitiveType
foreignType (UN typeName) =
  fromMaybe
    (error $ "Unsupported foreign type: " ++ unpack typeName)
    (HM.lookup typeName foreignTypes)

foreignTypes :: HM.HashMap Text PrimitiveType
foreignTypes = HM.fromList [("CIL_IntT", Int32)
                           ,("CIL_Str", String)
                           ,("CIL_Ptr", Object)
                           ,("CIL_Bool", Bool)
                           ,("CIL_Unit", Void)
                           ]
