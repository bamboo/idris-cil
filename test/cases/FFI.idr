{-
42
4.2
r
it works!
Void VoidFunction()
System.String exportedBoolToString(Boolean)
Void showMethod(System.Type, System.String)
before exported invocation
exported!
after exported invocation
True
00000000-0000-0000-0000-000000000000
-}
module Main

import CIL.FFI

AssemblyTy : CILTy
AssemblyTy = corlibTy "System.Reflection.Assembly"

Assembly : Type
Assembly = CIL AssemblyTy

MethodInfo : Type
MethodInfo = corlib "System.Reflection.MethodInfo"

instance IsA Object MethodInfo where {}

%inline
SystemMathMax : CILForeign
SystemMathMax = CILStatic (CILTyRef "mscorlib" "System.Math") "Max"

namespace System.Math.Int32
  Max : Int -> Int -> CIL_IO Int
  Max = invoke SystemMathMax (Int -> Int -> CIL_IO Int)

namespace System.Math.Float32
  Max : Double -> Double -> CIL_IO Double
  Max = invoke SystemMathMax (Double -> Double -> CIL_IO Double)

Substring : (this : String) -> (index : Int) -> (count : Int) -> CIL_IO String
Substring =
  invoke
    (CILInstance "Substring")
    (String -> Int -> Int -> CIL_IO String)

GetExecutingAssembly : CIL_IO Assembly
GetExecutingAssembly =
  invoke
    (CILStatic AssemblyTy "GetExecutingAssembly")
    (CIL_IO Assembly)

GetType : Assembly -> String -> Bool -> CIL_IO RuntimeType
GetType =
  invoke
    (CILInstance "GetType")
    (Assembly -> String -> Bool -> CIL_IO RuntimeType)

GetMethod : RuntimeType -> String -> CIL_IO MethodInfo
GetMethod =
  invoke
    (CILInstance "GetMethod")
    (RuntimeType -> String -> CIL_IO MethodInfo)

Invoke : MethodInfo -> Object -> ObjectArray -> CIL_IO Object
Invoke =
  invoke
    (CILInstance "Invoke")
    (MethodInfo -> Object -> ObjectArray -> CIL_IO Object)


namespace System.Text
  StringBuilder : Type
  StringBuilder = corlib "System.Text.StringBuilder"

  instance IsA Object StringBuilder where {}

  %inline
  invokeStringBuilder : String -> StringBuilder -> String -> CIL_IO StringBuilder
  invokeStringBuilder fn =
    invoke (CILInstance fn) (StringBuilder -> String -> CIL_IO StringBuilder)

  Append : StringBuilder -> String -> CIL_IO StringBuilder
  Append = invokeStringBuilder "Append"

  AppendLine : StringBuilder -> String -> CIL_IO StringBuilder
  AppendLine = invokeStringBuilder "AppendLine"


namespace System.Console
  Write : String -> CIL_IO ()
  Write =
    invoke (CILStatic (corlibTy "System.Console") "Write") (String -> CIL_IO ())


showMethod : RuntimeType -> String -> CIL_IO ()
showMethod t n = do m <- t `GetMethod` n
                    ToString m >>= putStrLn

GuidTy : CILTy
GuidTy = corlibTyVal "System.Guid"

Guid : Type
Guid = CIL $ GuidTy

instance IsA Object Guid where {}

NewGuid : CIL_IO Guid
NewGuid =
  invoke
    (CILStatic GuidTy "NewGuid")
    (CIL_IO Guid)

ParseGuid : String -> CIL_IO Guid
ParseGuid =
  invoke
    (CILStatic GuidTy "Parse")
    (String -> CIL_IO Guid)

EmptyGuid : CIL_IO Guid
EmptyGuid =
  invoke
    (CILStaticField GuidTy "Empty")
    (CIL_IO Guid)

testValueType : CIL_IO ()
testValueType = do
  guid  <- NewGuid
  guid' <- ParseGuid !(ToString guid)
  printLn !(Equals guid guid')
  ToString !EmptyGuid >>= putStrLn


--TODO: move to specific test case file
main : CIL_IO ()
main = do

  Max (the Int 42) (the Int 1) >>= printLn
  Max 4.2 1.0 >>= printLn
  Substring "idris" 2 1 >>= putStrLn

  sb <- new (CIL_IO StringBuilder)
  Append sb "it "
  AppendLine sb "works!"
  ToString sb >>= Write

  asm <- GetExecutingAssembly
  type <- GetType asm "TheExports" True

  for_ ["VoidFunction", "exportedBoolToString", "showMethod"] $
    showMethod type

  putStrLn "before exported invocation"
  exportedIO' <- type `GetMethod` "VoidFunction"
  Invoke exportedIO' !(nullOf Object) !(nullOf ObjectArray)
  putStrLn "after exported invocation"

  testValueType


exportedIO : CIL_IO ()
exportedIO = putStrLn "exported!"

exportedBoolToString : Bool -> String
exportedBoolToString = show

exports : FFI_Export FFI_CIL "TheExports" [] -- declare exported functions on a type with given name
exports =
  Fun exportedIO (CILExport "VoidFunction") $ -- export function under custom name
  Fun exportedBoolToString CILDefault $ -- export function under original name
  Fun showMethod CILDefault -- export signature containing CIL type
  End
