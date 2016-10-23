{-
42
4.2
it works!
True
00000000-0000-0000-0000-000000000000
Void VoidFunction()
System.String exportedBoolToString(Boolean)
Void showMethod(System.Type, System.String)
before exportedVoidIO
exported!
after exportedVoidIO
exportedBoolToStringIO True
exportedBoolToStringIO => True
Alan Kay
Kay, Alan
3
-}

module Main

import CIL.FFI
import CIL.FFI.Array
import Data.Vect

-- TODO: Move to rts
AssemblyTy : CILTy
AssemblyTy = corlibTy "System.Reflection.Assembly"

Assembly : Type
Assembly = CIL AssemblyTy

MethodInfo : Type
MethodInfo = corlib "System.Reflection.MethodInfo"

implementation IsA Object MethodInfo where {}

GetExecutingAssembly : CIL_IO Assembly
GetExecutingAssembly =
  invoke (CILStatic AssemblyTy "GetExecutingAssembly")
         (CIL_IO Assembly)

GetType : Assembly -> String -> Bool -> CIL_IO RuntimeType
GetType =
  invoke (CILInstance "GetType")
         (Assembly -> String -> Bool -> CIL_IO RuntimeType)

GetMethod : RuntimeType -> String -> CIL_IO (Nullable MethodInfo)
GetMethod =
  invoke (CILInstance "GetMethod")
         (RuntimeType -> String -> CIL_IO (Nullable MethodInfo))

Invoke : Nullable MethodInfo -> Nullable Object -> Nullable ObjectArray -> CIL_IO Object
Invoke =
  invoke (CILInstance "Invoke")
         (Nullable MethodInfo -> Nullable Object -> Nullable ObjectArray -> CIL_IO Object)

%inline
SystemMathMax : CILForeign
SystemMathMax = CILStatic (CILTyRef "mscorlib" "System.Math") "Max"

namespace System.Math.Int32
  Max : Int -> Int -> CIL_IO Int
  Max = invoke SystemMathMax (Int -> Int -> CIL_IO Int)

namespace System.Math.Double
  Max : Double -> Double -> CIL_IO Double
  Max = invoke SystemMathMax (Double -> Double -> CIL_IO Double)

namespace System.Text
  StringBuilder : Type
  StringBuilder = corlib "System.Text.StringBuilder"

  implementation IsA Object StringBuilder where {}

  %inline
  invokeStringBuilder : String -> StringBuilder -> String -> CIL_IO StringBuilder
  invokeStringBuilder fn = invoke (CILInstance fn) (StringBuilder -> String -> CIL_IO StringBuilder)

  Append : StringBuilder -> String -> CIL_IO StringBuilder
  Append = invokeStringBuilder "Append"

  AppendLine : StringBuilder -> String -> CIL_IO StringBuilder
  AppendLine = invokeStringBuilder "AppendLine"


GuidTy : CILTy
GuidTy = corlibTyVal "System.Guid"

Guid : Type
Guid = CIL $ GuidTy

implementation IsA Object Guid where {}

NewGuid : CIL_IO Guid
NewGuid =
  invoke (CILStatic GuidTy "NewGuid")
         (CIL_IO Guid)

ParseGuid : String -> CIL_IO Guid
ParseGuid =
  invoke (CILStatic GuidTy "Parse")
         (String -> CIL_IO Guid)

EmptyGuid : CIL_IO Guid
EmptyGuid =
  invoke (CILStaticField GuidTy "Empty")
         (CIL_IO Guid)

%inline
objectArrayFor : Vect _ Object -> CIL_IO ObjectArray
objectArrayFor xs = arrayOf CILTyObj xs

testValueType : CIL_IO ()
testValueType = do
  guid  <- NewGuid
  guid' <- ParseGuid !(ToString guid)
  printLn !(Equals guid guid')
  ToString !EmptyGuid >>= putStrLn

testExportedVoidFunction : RuntimeType -> CIL_IO ()
testExportedVoidFunction type = do
  putStrLn "before exportedVoidIO"
  exportedVoidIO' <- type `GetMethod` "VoidFunction"
  ret <- Invoke exportedVoidIO' null null
  putStrLn "after exportedVoidIO"

testExportedBoolToStringIO : RuntimeType -> CIL_IO ()
testExportedBoolToStringIO type = do
  exportedBoolToStringIO' <- type `GetMethod` "exportedBoolToStringIO"
  ret <- Invoke exportedBoolToStringIO' null (asNullable !(objectArrayFor [asObject True]))
  putStrLn $ "exportedBoolToStringIO => " ++ !(ToString ret)

record Person where
  constructor MkPerson
  firstName, lastName : String

-- And now for something completely different...
-- Let's use the IMPORTING FFI to test the EXPORTING FFI

||| Descriptor for the type hosting all exported functions.
TheExportsTy : CILTy
TheExportsTy = CILTyRef "" "TheExports"

||| The foreign view of the exported type Person
||| is a struct with a single field `ptr`.
ExportedPerson : Type
ExportedPerson = CIL $ CILTyVal "" "Person"

||| Converts a foreign reference to an exported data type
||| into its internal representation.
unForeign : ExportedPerson -> CIL_IO Person
unForeign ep = do
 ptr <- invoke (CILInstanceField "ptr") (ExportedPerson -> CIL_IO Ptr) ep
 pure $ believe_me ptr

||| Invokes the exported function `createPerson` via the FFI.
invokeCreatePerson : String -> String -> CIL_IO ExportedPerson
invokeCreatePerson =
  invoke (CILStatic TheExportsTy "createPerson")
         (String -> String -> CIL_IO ExportedPerson)

%inline
invokeAccessor : String -> ExportedPerson -> CIL_IO String
invokeAccessor n =
  invoke (CILStatic TheExportsTy n)
         (ExportedPerson -> CIL_IO String)

testExportedRecord : CIL_IO ()
testExportedRecord = do
  -- exercise the foreign view of the record
  ep <- invokeCreatePerson "Alan" "Kay"
  putStrLn $ !(invokeAccessor "firstName" ep) ++ " " ++ !(invokeAccessor "lastName" ep)
  -- internal view should work just the same
  p  <- unForeign ep
  putStrLn $ lastName p ++ ", " ++ firstName p

testOverloadedStaticMethod : CIL_IO ()
testOverloadedStaticMethod = do
  Max (the Int 42) (the Int 1) >>= printLn
  Max 4.2 1.0 >>= printLn

testInstanceMethods : CIL_IO ()
testInstanceMethods = do
  sb <- new (CIL_IO StringBuilder)
  Append sb "it "
  AppendLine sb "works!"
  ToString sb >>= Write

showMethod : RuntimeType -> String -> CIL_IO ()
showMethod t n = do
  m <- t `GetMethod` n
  nullable (pure "method not found") ToString m >>= putStrLn

testBoxingUnboxing : RuntimeType -> CIL_IO ()
testBoxingUnboxing type = do
  meth <- type `GetMethod` "exportedIncInt"
  ret <- Invoke meth null (asNullable !(objectArrayFor [asObject 2]))
  ToString ret >>= putStrLn

main : CIL_IO ()
main = do
  testOverloadedStaticMethod
  testInstanceMethods
  testValueType

  asm <- GetExecutingAssembly
  type <- GetType asm "TheExports" True
  for_ (the (List _) ["VoidFunction", "exportedBoolToString", "showMethod"]) $
    showMethod type

  testExportedVoidFunction type
  testExportedBoolToStringIO type
  testExportedRecord
  testBoxingUnboxing type

-- Exports

createPerson : String -> String -> Person
createPerson = MkPerson

exportedVoidIO : CIL_IO ()
exportedVoidIO = putStrLn "exported!"

exportedBoolToString : Bool -> String
exportedBoolToString = show

exportedBoolToStringIO : Bool -> CIL_IO String
exportedBoolToStringIO b = do
  putStrLn $ "exportedBoolToStringIO " ++ show b
  pure $ show b

exportedIncInt : Int -> Int
exportedIncInt i = i + 1

exports : FFI_Export FFI_CIL "TheExports" [] -- declare exported functions on a type with given name
exports =
  Data Person "Person" $
  Fun createPerson CILDefault $ -- export function under original name
  Fun firstName CILDefault $ -- record field accessors are just functions and can be as easily exported
  Fun lastName CILDefault $
  Fun exportedVoidIO (CILExport "VoidFunction") $ -- export function under custom name
  Fun exportedBoolToString CILDefault $
  Fun exportedBoolToStringIO CILDefault $ -- export IO with return value
  Fun exportedIncInt CILDefault $ -- pass and get back value type
  Fun showMethod CILDefault -- export signature containing CIL type
  End

-- Local Variables:
-- idris-load-packages: ("cil")
-- End:
