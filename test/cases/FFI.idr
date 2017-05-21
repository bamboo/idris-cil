{-
42
4.2
it works!
True
00000000-0000-0000-0000-000000000000
VoidFunction : Void
exportedBoolToString : (p0 : Boolean) -> String
printMethod : (t : Type) -> (n : String) -> Void
take5 : (defaultValue : String) -> (flag : Boolean) -> (c : Char) -> (i : Int32) -> (d : Double) -> String
before exportedVoidIO
exported!
after exportedVoidIO
exportedBoolToStringIO True
exportedBoolToStringIO => True
Alan Kay
Kay, Alan
3
createPerson, exportedBoolToString, exportedBoolToStringIO, exportedIncInt, firstName, lastName, printMethod, take5, VoidFunction
-}

module Main

import CIL.System.Reflection
import Data.Vect

%inline
objectArrayFor : Vect _ Object -> CIL_IO ObjectArray
objectArrayFor xs = arrayOf CILTyObj xs

testValueType : CIL_IO ()
testValueType = do
  guid  <- Guid.NewGuid
  guid' <- Guid.Parse !(ToString guid)
  printLn !(Equals guid guid')
  ToString !Guid.Empty >>= putStrLn

invokeStaticMethod : RuntimeType -> String -> Maybe ObjectArray -> CIL_IO (Maybe Object)
invokeStaticMethod type methodName args =
  maybe (pure Nothing) (\method => Just <$> Invoke method Nothing args) !(type `GetMethod` methodName)

testExportedVoidFunction : RuntimeType -> CIL_IO ()
testExportedVoidFunction type = do
  putStrLn "before exportedVoidIO"
  invokeStaticMethod type "VoidFunction" Nothing
  putStrLn "after exportedVoidIO"

testExportedBoolToStringIO : RuntimeType -> CIL_IO ()
testExportedBoolToStringIO type = do
  ret <- invokeStaticMethod type "exportedBoolToStringIO" (Just !(objectArrayFor [upcast True]))
  retString <- maybe (pure "ERROR") ToString ret
  putStrLn $ "exportedBoolToStringIO => " ++ retString

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
  invokeStatic TheExportsTy "createPerson" (String -> String -> CIL_IO ExportedPerson)

%inline
invokeAccessor : String -> ExportedPerson -> CIL_IO String
invokeAccessor n =
  invokeStatic TheExportsTy n (ExportedPerson -> CIL_IO String)

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

showParameter : ParameterInfo -> CIL_IO String
showParameter p = do
  n <- get_Name p
  t <- get_ParameterType p >>= get_Name
  pure $ "(" ++ n ++ " : " ++ t ++ ")"

showMethod : MethodInfo -> CIL_IO String
showMethod m = do
  name <- get_Name m
  ps   <- foldr (\p, acc => (:: acc) <$> showParameter p) (the (List _) []) !(GetParameters m)
  ret  <- get_ReturnType m >>= get_Name
  let sig = ps ++ [ret]
  pure $ name ++ " : " ++ (concat . intersperse " -> " $ sig)

printMethod : RuntimeType -> String -> CIL_IO ()
printMethod t n = do
  m <- t `GetMethod` n
  maybe (pure "method not found") showMethod m >>= putStrLn

testBoxingUnboxing : RuntimeType -> CIL_IO ()
testBoxingUnboxing type = do
  ret <- invokeStaticMethod type "exportedIncInt" (Just !(objectArrayFor [upcast 2]))
  maybe (pure "ERROR") ToString ret >>= putStrLn

getExportsType : CIL_IO RuntimeType
getExportsType = do
  asm <- GetExecutingAssembly
  GetType asm "TheExports" True

testGetMethodsWithBindingFlags : CIL_IO ()
testGetMethodsWithBindingFlags = do
  type <- getExportsType
  methods <- type `GetMethods` (Public + Static)
  methodNames <- foldr (\m, acc => (:: acc) <$> get_Name m) (the (List String) []) methods
  putStrLn . concat . intersperse ", " . sort $ methodNames

main : CIL_IO ()
main = do
  testOverloadedStaticMethod
  testInstanceMethods
  testValueType

  type <- getExportsType
  for_ (the (List _) ["VoidFunction", "exportedBoolToString", "printMethod", "take5"]) $
    printMethod type

  testExportedVoidFunction type
  testExportedBoolToStringIO type
  testExportedRecord
  testBoxingUnboxing type
  testGetMethodsWithBindingFlags

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

parameters (defaultValue: String, flag: Bool)

  take5 : Char -> Int -> Double -> String
  take5 c i d = if flag then defaultValue else show $ cast (ord c) + cast i + d

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
  Fun printMethod CILDefault $ -- export signature containing CIL type
  Fun take5 CILDefault
  End

-- Local Variables:
-- idris-load-packages: ("cil")
-- End:
