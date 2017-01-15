module CIL.System.Reflection

import public CIL.FFI
import public CIL.FFI.Array

%default total

%access public export

AssemblyTy : CILTy
AssemblyTy = corlibTy "System.Reflection.Assembly"

Assembly : Type
Assembly = CIL AssemblyTy

MethodInfoTy : CILTy
MethodInfoTy = corlibTy "System.Reflection.MethodInfo"

MethodInfo : Type
MethodInfo = CIL MethodInfoTy

ParameterInfoTy : CILTy
ParameterInfoTy = corlibTy "System.Reflection.ParameterInfo"

ParameterInfo : Type
ParameterInfo = CIL ParameterInfoTy

ParameterInfoArray : Type
ParameterInfoArray = TypedArrayOf ParameterInfoTy

TypeArray : Type
TypeArray = TypedArrayOf RuntimeTypeTy

implementation IsA Object MethodInfo where {}
implementation IsA Object Assembly where {}

GetExecutingAssembly : CIL_IO Assembly
GetExecutingAssembly =
  invoke (CILStatic AssemblyTy "GetExecutingAssembly")
         (CIL_IO Assembly)

namespace Assembly

    GetType : Assembly -> String -> Bool -> CIL_IO RuntimeType
    GetType =
        invoke (CILInstance "GetType")
               (Assembly -> String -> Bool -> CIL_IO RuntimeType)

    GetExportedTypes : Assembly -> CIL_IO TypeArray
    GetExportedTypes =
        invoke (CILInstance "GetExportedTypes")
               (Assembly -> CIL_IO TypeArray)

namespace RuntimeType

  get_Name : RuntimeType -> CIL_IO String
  get_Name = invoke (CILInstance "get_Name") (RuntimeType -> CIL_IO String)

  GetMethod : RuntimeType -> String -> CIL_IO (Maybe MethodInfo)
  GetMethod =
    invoke (CILInstance "GetMethod")
           (RuntimeType -> String -> CIL_IO (Maybe MethodInfo))

namespace MethodInfo

  get_Name : MethodInfo -> CIL_IO String
  get_Name = invoke (CILInstance "get_Name") (MethodInfo -> CIL_IO String)

  GetParameters : MethodInfo -> CIL_IO ParameterInfoArray
  GetParameters = invoke (CILInstance "GetParameters") (MethodInfo -> CIL_IO ParameterInfoArray)

  get_ReturnType : MethodInfo -> CIL_IO RuntimeType
  get_ReturnType = invoke (CILInstance "get_ReturnType") (MethodInfo -> CIL_IO RuntimeType)

  Invoke : MethodInfo -> Maybe Object -> Maybe ObjectArray -> CIL_IO Object
  Invoke =
    invoke (CILInstance "Invoke")
            (MethodInfo -> Maybe Object -> Maybe ObjectArray -> CIL_IO Object)

namespace ParameterInfo

  get_Name : ParameterInfo -> CIL_IO String
  get_Name = invoke (CILInstance "get_Name") (ParameterInfo -> CIL_IO String)

  get_ParameterType : ParameterInfo -> CIL_IO RuntimeType
  get_ParameterType = invoke (CILInstance "get_ParameterType") (ParameterInfo -> CIL_IO RuntimeType)

