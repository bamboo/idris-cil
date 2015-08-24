{-
42
r
Void VoidFunction()
System.String exportedBoolToString(Boolean)
-}
module Main

data RuntimeType = MkRuntimeType
data Assembly    = MkAssembly
data MethodInfo  = MkMethodInfo

mutual
  -- TODO: how to get rid of these hardcoded constructors?
  data CIL_RefTypes : String -> Type -> Type where
       CIL_Assembly : CIL_RefTypes "System.Reflection.Assembly" Assembly
       CIL_Type     : CIL_RefTypes "System.Type" RuntimeType
       CIL_Method   : CIL_RefTypes "System.Reflection.MethodInfo" MethodInfo

  data CIL_IntTypes  : Type -> Type where
       CIL_IntChar   : CIL_IntTypes Char
       CIL_IntNative : CIL_IntTypes Int

  data CIL_Types : Type -> Type where
       CIL_Str   : CIL_Types String
       CIL_Float : CIL_Types Float
       CIL_Ptr   : CIL_Types Ptr
       CIL_Bool  : CIL_Types Bool
       CIL_Unit  : CIL_Types ()
       CIL_IntT  : CIL_IntTypes i -> CIL_Types i
       CIL_RefT  : CIL_RefTypes n t -> CIL_Types t
  --   CIL_FnT   : CIL_FnTypes a -> CIL_Types (CilFn a)

  -- data CilFn t = MkCilFn t
  -- data CIL_FnTypes : Type -> Type where
  --      CIL_Fn      : CIL_Types s -> CIL_FnTypes t -> CIL_FnTypes (s -> t)
  --      CIL_FnIO    : CIL_Types t -> CIL_FnTypes (IO' l t)
  --      CIL_FnBase  : CIL_Types t -> CIL_FnTypes t

FFI_CIL : FFI
FFI_CIL = MkFFI CIL_Types String Type

CIL_IO : Type -> Type
CIL_IO a = IO' FFI_CIL a

exportedIO : CIL_IO ()
exportedIO = putStrLn "exported!"

exportedBoolToString : Bool -> String
exportedBoolToString = show

exports : FFI_Export FFI_CIL "" [] -- export under current module's namespace
exports = Fun exportedIO "VoidFunction" $ -- export function under different name
          Fun exportedBoolToString "" $ -- export function under original name
          End

systemMax : Int -> Int -> CIL_IO Int
systemMax =
  foreign FFI_CIL
    "[mscorlib]System.Math::Max"
    (Int -> Int -> CIL_IO Int)

substring : String -> Int -> Int -> CIL_IO String
substring this index count =
  foreign FFI_CIL
    "instance [mscorlib]System.String::Substring"
    (String -> Int -> Int -> CIL_IO String)
    this index count

getExecutingAssembly : CIL_IO Assembly
getExecutingAssembly =
  foreign FFI_CIL
    "[mscorlib]System.Reflection.Assembly::GetExecutingAssembly"
    (CIL_IO Assembly)

getType : Assembly -> String -> Bool -> CIL_IO RuntimeType
getType =
  foreign FFI_CIL
    "instance [mscorlib]System.Reflection.Assembly::GetType"
    (Assembly -> String -> Bool -> CIL_IO RuntimeType)

getMethod : RuntimeType -> String -> CIL_IO MethodInfo
getMethod =
  foreign FFI_CIL
    "instance [mscorlib]System.Type::GetMethod"
    (RuntimeType -> String -> CIL_IO MethodInfo)

MethodInfoToString : MethodInfo -> CIL_IO String
MethodInfoToString =
  foreign FFI_CIL
    "instance [mscorlib]System.Object::ToString"
    (MethodInfo -> CIL_IO String)

main : CIL_IO ()
main = do systemMax 42 1 >>= printLn
          substring "idris" 2 1 >>= putStrLn
          asm <- getExecutingAssembly
          type <- getType asm "Main" True
          for_ ["VoidFunction", "exportedBoolToString"] $
            showMethod type
  where showMethod : RuntimeType -> String -> CIL_IO ()
        showMethod t n = do m <- t `getMethod` n
                            MethodInfoToString m >>= putStrLn
