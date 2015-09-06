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
-}
module Main

||| The universe of foreign CIL types.
data CILTy =
  ||| a foreign reference type
  CILTyRef String String |
  ||| a foreign value type
  CILTyVal String String |
  ||| a foreign array type
  CILTyArr CILTy

instance Eq CILTy where
  (CILTyRef ns t) == (CILTyRef ns' t') = ns == ns' && t == t'
  (CILTyVal ns t) == (CILTyVal ns' t') = ns == ns' && t == t'
  _               == _                 = False

||| A foreign CIL type.
data CIL   : CILTy -> Type where
     MkCIL : (ty : CILTy) -> CIL ty

||| A foreign descriptor.
data CILForeign =
  ||| Call the named instance method.
  CILInstance String |
  ||| Call the named static method of the given foreign type.
  CILStatic CILTy String |
  ||| Call a constructor to instantiate an object.
  CILConstructor |
  ||| Export a function under the given name.
  CILExport String |
  ||| Export a function under its original name.
  CILDefault |
  ||| null
  CILNull

mutual
  data CIL_IntTypes  : Type -> Type where
       CIL_IntChar   : CIL_IntTypes Char
       CIL_IntNative : CIL_IntTypes Int

  data CIL_Types : Type -> Type where
       CIL_Str   : CIL_Types String
       CIL_Float : CIL_Types Double
       CIL_Ptr   : CIL_Types Ptr
       CIL_Bool  : CIL_Types Bool
       CIL_Unit  : CIL_Types ()
       CIL_IntT  : CIL_IntTypes i -> CIL_Types i
       CIL_CILT  : CIL_Types (CIL ty)
  --   CIL_FnT   : CIL_FnTypes a -> CIL_Types (CilFn a)

  -- data CilFn t = MkCilFn t
  -- data CIL_FnTypes : Type -> Type where
  --      CIL_Fn      : CIL_Types s -> CIL_FnTypes t -> CIL_FnTypes (s -> t)
  --      CIL_FnIO    : CIL_Types t -> CIL_FnTypes (IO' l t)
  --      CIL_FnBase  : CIL_Types t -> CIL_FnTypes t

FFI_CIL : FFI
FFI_CIL = MkFFI CIL_Types CILForeign Type

CIL_IO : Type -> Type
CIL_IO a = IO' FFI_CIL a

invoke : CILForeign -> (ty : Type) ->
         {auto fty : FTy FFI_CIL [] ty} -> ty
invoke ffi ty = foreign FFI_CIL ffi ty

%inline
new : (ty : Type) ->
      {auto fty : FTy FFI_CIL [] ty} -> ty
new ty = invoke CILConstructor ty

%inline
nullOf : (ty: Type) ->
         {auto fty : FTy FFI_CIL [] (CIL_IO ty)} -> CIL_IO ty
nullOf ty = invoke CILNull (CIL_IO ty)

%inline
corlibTy : String -> CILTy
corlibTy = CILTyRef "mscorlib"

%inline
corlib : String -> Type
corlib = CIL . corlibTy

ObjectTy : CILTy
ObjectTy = corlibTy "System.Object"

Object : Type
Object = CIL ObjectTy

AssemblyTy : CILTy
AssemblyTy = corlibTy "System.Reflection.Assembly"

Assembly : Type
Assembly = CIL AssemblyTy

RuntimeType : Type
RuntimeType = corlib "System.Type"

MethodInfo : Type
MethodInfo = corlib "System.Reflection.MethodInfo"

ObjectArray : Type
ObjectArray = CIL (CILTyArr ObjectTy)

-- inheritance can be encoded as class instances or implicit conversions
class IsA a b where {}
instance IsA Object MethodInfo where {}

-- implicit MethodInfoIsAObject : MethodInfo -> Object
-- MethodInfoIsAObject m = (believe_me m)

%inline
SystemMathMax : CILForeign
SystemMathMax = CILStatic (CILTyRef "mscorlib" "System.Math") "Max"

namespace System.Math.Int32
  Max : Int -> Int -> CIL_IO Int
  Max = invoke SystemMathMax (Int -> Int -> CIL_IO Int)

namespace System.Math.Float32
  Max : Double -> Double -> CIL_IO Double
  Max = invoke SystemMathMax (Double -> Double -> CIL_IO Double)

Substring : String -> Int -> Int -> CIL_IO String
Substring this index count =
  invoke
    (CILInstance "Substring")
    (String -> Int -> Int -> CIL_IO String)
    this index count

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

ToString : IsA Object o => o -> CIL_IO String
ToString obj =
  invoke
    (CILInstance "ToString")
    (Object -> CIL_IO String)
    (believe_me obj)


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

main : CIL_IO ()
main = do Max (the Int 42) (the Int 1) >>= printLn
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

exportedIO : CIL_IO ()
exportedIO = putStrLn "exported!"

exportedBoolToString : Bool -> String
exportedBoolToString = show

exports : FFI_Export FFI_CIL "TheExports" [] -- declare exported functions on a type with given name
exports =
  Fun exportedIO (CILExport "VoidFunction") $ -- export function with custom signature
  Fun exportedBoolToString CILDefault $ -- export function under original name and signature
  Fun showMethod CILDefault -- export signature containing CIL type
  End
