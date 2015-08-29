{-
42
4.2
r
it works!
Void VoidFunction()
System.String exportedBoolToString(Boolean)
-}
module Main

namespace FFI_CIL
  data Raw : Type -> Type where
    MkRaw : (x: t) -> FFI_CIL.Raw t

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
       CIL_AnyT  : FFI_CIL.Raw t -> CIL_Types t
  --   CIL_FnT   : CIL_FnTypes a -> CIL_Types (CilFn a)

  -- data CilFn t = MkCilFn t
  -- data CIL_FnTypes : Type -> Type where
  --      CIL_Fn      : CIL_Types s -> CIL_FnTypes t -> CIL_FnTypes (s -> t)
  --      CIL_FnIO    : CIL_Types t -> CIL_FnTypes (IO' l t)
  --      CIL_FnBase  : CIL_Types t -> CIL_FnTypes t

data CILTy = CILTyObj
           | CILTyStr
           | CILTyInt32
           | CILTyFloat32
           | CILTyBool
           | CILTyVoid
           | CILTyRef String String
           | CILTyVal String String

CILSig : Type
CILSig = List CILTy

data CILForeign = CILInstance    CILTy String CILSig CILTy
                | CILStatic      CILTy String CILSig CILTy
                | CILConstructor CILTy CILSig
                | CILExport      String CILSig CILTy
                | CILDefault

FFI_CIL : FFI
FFI_CIL = MkFFI CIL_Types CILForeign Type

CIL_IO : Type -> Type
CIL_IO a = IO' FFI_CIL a

interpCILTy : CILTy -> Type
interpCILTy CILTyInt32   = Int
interpCILTy CILTyStr     = String
interpCILTy CILTyBool    = Bool
interpCILTy CILTyVoid    = Unit
interpCILTy CILTyFloat32 = Double
interpCILTy _            = Ptr

interpSig : List CILTy -> CILTy -> Type
interpSig sig ret = interp sig (interpCILTy ret)
  where interp : List CILTy -> Type -> Type
        interp [] r = CIL_IO r
        interp (x::xs) r = interpCILTy x -> interp xs r

%inline
interpTy : CILForeign -> Type
interpTy (CILInstance decl _ sig ret) = interpCILTy decl -> interpSig sig ret
interpTy (CILStatic   _    _ sig ret) = interpSig sig ret
interpTy _ = Unit

invoke : (ffi : CILForeign) ->
         {auto fty : FTy FFI_CIL [] (interpTy ffi)} -> (interpTy ffi)
invoke ffi = foreign FFI_CIL ffi (interpTy ffi)

new : (decl: CILTy) -> (sig: CILSig) ->
      {auto fty : FTy FFI_CIL [] (interpSig sig decl)} -> (interpSig sig decl)
new decl sig = foreign FFI_CIL (CILConstructor decl sig) (interpSig sig decl)

exportedIO : CIL_IO ()
exportedIO = putStrLn "exported!"

exportedBoolToString : Bool -> String
exportedBoolToString = show

exports : FFI_Export FFI_CIL "" [] -- export under current module's namespace
exports =
  Fun exportedIO (CILExport "VoidFunction" [] CILTyVoid) $ -- export function with custom signature
  Fun exportedBoolToString CILDefault $ -- export function under original name and signature
  End

-- Each CIL type is represented by a unique data type (optional) and a CILTy instance
data RuntimeType = MkRuntimeType

%inline
RuntimeTypeT : CILTy
RuntimeTypeT = CILTyRef "mscorlib" "System.Type"

data Assembly = MkAssembly

%inline
AssemblyT : CILTy
AssemblyT = CILTyRef "mscorlib" "System.Reflection.Assembly"

data MethodInfo = MkMethodInfo

%inline
MethodInfoT : CILTy
MethodInfoT = CILTyRef "mscorlib" "System.Reflection.MethodInfo"

namespace System.Math.Int32
  Max : Int -> Int -> CIL_IO Int
  Max =
    invoke (CILStatic (CILTyRef "mscorlib" "System.Math") "Max" [CILTyInt32, CILTyInt32] CILTyInt32)

namespace System.Math.Float32
  Max : Double -> Double -> CIL_IO Double
  Max =
    invoke (CILStatic (CILTyRef "mscorlib" "System.Math") "Max" [CILTyFloat32, CILTyFloat32] CILTyFloat32)

substring : String -> Int -> Int -> CIL_IO String
substring this index count =
  invoke
    (CILInstance CILTyStr "Substring" [CILTyInt32, CILTyInt32] CILTyStr)
    this index count

getExecutingAssembly : CIL_IO Assembly
getExecutingAssembly =
  foreign FFI_CIL
    (CILStatic AssemblyT "GetExecutingAssembly" [] AssemblyT)
    (CIL_IO Assembly)

getType : Assembly -> String -> Bool -> CIL_IO RuntimeType
getType =
  foreign FFI_CIL
    (CILInstance AssemblyT "GetType" [CILTyStr, CILTyBool] RuntimeTypeT)
    (Assembly -> String -> Bool -> CIL_IO RuntimeType)

getMethod : RuntimeType -> String -> CIL_IO MethodInfo
getMethod =
  foreign FFI_CIL
    (CILInstance RuntimeTypeT "GetMethod" [CILTyStr] MethodInfoT)
    (RuntimeType -> String -> CIL_IO MethodInfo)

toString : o -> CIL_IO String
toString obj =
  foreign FFI_CIL
    (CILInstance CILTyObj "ToString" [] CILTyStr)
    (o -> CIL_IO String)
    obj


namespace System.Text.StringBuilder
  StringBuilderT : CILTy
  StringBuilderT = CILTyRef "mscorlib" "System.Text.StringBuilder"

  %inline
  invokeStringBuilder : String -> Ptr -> String -> CIL_IO Ptr
  invokeStringBuilder fn =
    invoke (CILInstance StringBuilderT fn [CILTyStr] StringBuilderT)

  Append : Ptr -> String -> CIL_IO Ptr
  Append = invokeStringBuilder "Append"

  AppendLine : Ptr -> String -> CIL_IO Ptr
  AppendLine = invokeStringBuilder "AppendLine"


namespace System.Console
  Write : String -> CIL_IO ()
  Write =
    invoke (CILStatic (CILTyRef "mscorlib" "System.Console") "Write" [CILTyStr] CILTyVoid)


main : CIL_IO ()
main = do Max (the Int 42) (the Int 1) >>= printLn
          Max 4.2 1.0 >>= printLn
          substring "idris" 2 1 >>= putStrLn

          sb <- new StringBuilderT []
          Append sb "it "
          AppendLine sb "works!"
          toString sb >>= Write

          asm <- getExecutingAssembly
          type <- getType asm "Main" True
          for_ ["VoidFunction", "exportedBoolToString"] $
            showMethod type
  where showMethod : RuntimeType -> String -> CIL_IO ()
        showMethod t n = do m <- t `getMethod` n
                            toString m >>= putStrLn
