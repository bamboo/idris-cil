module CIL.System.Reflection

import public CIL.FFI
import public CIL.FFI.Array

import CIL.Elab.Enums

%language ElabReflection

%default total

%access public export

AppDomainTy : CILTy
AppDomainTy = corlibTy "System.AppDomain"

AppDomain : Type
AppDomain = CIL AppDomainTy

AssemblyTy : CILTy
AssemblyTy = corlibTy "System.Reflection.Assembly"

Assembly : Type
Assembly = CIL AssemblyTy

MethodInfoTy : CILTy
MethodInfoTy = corlibTy "System.Reflection.MethodInfo"

MethodInfo : Type
MethodInfo = CIL MethodInfoTy

MethodInfoArray : Type
MethodInfoArray = TypedArrayOf MethodInfoTy

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

BindingFlagsTy : CILTy
BindingFlagsTy = corlibTyVal "System.Reflection.BindingFlags"

namespace BindingFlags

  %runElab
    cilEnum BindingFlags BindingFlagsTy Bits32
            [ cilField Static 0x08
            , cilField Public 0x10
            ]

namespace Assembly

  GetExecutingAssembly : CIL_IO Assembly
  GetExecutingAssembly = invokeStatic AssemblyTy "GetExecutingAssembly" (CIL_IO Assembly)

  GetType : Assembly -> String -> Bool -> CIL_IO RuntimeType
  GetType = invokeInstance "GetType" (Assembly -> String -> Bool -> CIL_IO RuntimeType)

  GetExportedTypes : Assembly -> CIL_IO TypeArray
  GetExportedTypes = invokeInstance "GetExportedTypes" (Assembly -> CIL_IO TypeArray)

  Load : String -> CIL_IO Assembly
  Load = invokeStatic AssemblyTy "Load" (String -> CIL_IO Assembly)

  ReflectionOnlyLoadFrom : String -> CIL_IO Assembly
  ReflectionOnlyLoadFrom = invokeStatic AssemblyTy "ReflectionOnlyLoadFrom" (String -> CIL_IO Assembly)

namespace RuntimeType

  get_Name : RuntimeType -> CIL_IO String
  get_Name = invokeInstance "get_Name" (RuntimeType -> CIL_IO String)

  GetMethod : RuntimeType -> String -> CIL_IO (Maybe MethodInfo)
  GetMethod = invokeInstance "GetMethod" (RuntimeType -> String -> CIL_IO (Maybe MethodInfo))

  GetMethods : RuntimeType -> BindingFlags -> CIL_IO MethodInfoArray
  GetMethods = invokeInstance "GetMethods" (RuntimeType -> BindingFlags -> CIL_IO MethodInfoArray)

namespace MethodInfo

  get_Name : MethodInfo -> CIL_IO String
  get_Name = invokeInstance "get_Name" (MethodInfo -> CIL_IO String)

  GetParameters : MethodInfo -> CIL_IO ParameterInfoArray
  GetParameters = invokeInstance "GetParameters" (MethodInfo -> CIL_IO ParameterInfoArray)

  get_ReturnType : MethodInfo -> CIL_IO RuntimeType
  get_ReturnType = invokeInstance "get_ReturnType" (MethodInfo -> CIL_IO RuntimeType)

  Invoke : MethodInfo -> Maybe Object -> Maybe ObjectArray -> CIL_IO Object
  Invoke = invokeInstance "Invoke" (MethodInfo -> Maybe Object -> Maybe ObjectArray -> CIL_IO Object)

namespace ParameterInfo

  get_Name : ParameterInfo -> CIL_IO String
  get_Name = invokeInstance "get_Name" (ParameterInfo -> CIL_IO String)

  get_ParameterType : ParameterInfo -> CIL_IO RuntimeType
  get_ParameterType = invokeInstance "get_ParameterType" (ParameterInfo -> CIL_IO RuntimeType)

namespace AppDomain

  CurrentDomain : CIL_IO AppDomain
  CurrentDomain = invokeStatic AppDomainTy "get_CurrentDomain" (CIL_IO AppDomain)

  GetAssemblies : AppDomain -> CIL_IO (TypedArrayOf AssemblyTy)
  GetAssemblies = invokeInstance "GetAssemblies" (AppDomain -> CIL_IO (TypedArrayOf AssemblyTy))
