module CIL.FFI

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
  ||| Read the value of the named static field of the given foreign type.
  CILStaticField CILTy String |
  ||| Call a constructor to instantiate an object.
  CILConstructor |
  ||| Loads the given runtime type.
  CILTypeOf CILTy |
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
corlibTyVal : String -> CILTy
corlibTyVal = CILTyVal "mscorlib"

%inline
corlib : String -> Type
corlib = CIL . corlibTy

ObjectTy : CILTy
ObjectTy = corlibTy "System.Object"

Object : Type
Object = CIL ObjectTy

ObjectArray : Type
ObjectArray = CIL (CILTyArr ObjectTy)

RuntimeType : Type
RuntimeType = corlib "System.Type"

%inline
typeOf : CILTy -> CIL_IO RuntimeType
typeOf t = invoke (CILTypeOf t) (CIL_IO RuntimeType)

-- inheritance can be encoded as class instances or implicit conversions
class IsA a b where {}

instance IsA Object Object where {}
instance IsA Object RuntimeType where {}

ToString : IsA Object o => o -> CIL_IO String
ToString obj =
  invoke
    (CILInstance "ToString")
    (Object -> CIL_IO String)
    (believe_me obj)

Equals : IsA Object a => a -> a -> CIL_IO Bool
Equals x y =
  invoke
    (CILInstance "Equals")
    (Object -> Object -> CIL_IO Bool)
    (believe_me x) (believe_me y)

ArrayTy : CILTy
ArrayTy = corlibTy "System.Array"

Array : Type
Array = CIL ArrayTy

CreateInstance : RuntimeType -> Int -> CIL_IO Array
CreateInstance =
  invoke
    (CILStatic ArrayTy "CreateInstance")
    (RuntimeType -> Int -> CIL_IO Array)

SetValue : Array -> Object -> Int -> CIL_IO ()
SetValue =
  invoke
    (CILInstance "SetValue")
    (Array -> Object -> Int -> CIL_IO ())

fromList : List a -> CIL_IO ObjectArray
fromList [v] = do
  array <- CreateInstance !(typeOf ObjectTy) 1
  SetValue array (believe_me v) 0
  return $ believe_me array
