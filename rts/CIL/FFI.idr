module CIL.FFI

%default total

%access public

||| The universe of foreign CIL types.
data CILTy =
  ||| a foreign reference type
  CILTyRef String String |
  ||| a foreign value type
  CILTyVal String String |
  ||| a foreign array type
  CILTyArr CILTy |
  ||| a foreign generic type
  CILTyGen CILTy (List CILTy) |
  ||| a foreign generic parameter
  CILTyGenParam String

instance Eq CILTy where
  (CILTyGen def args) == (CILTyGen def' args') = def  == def' && assert_total (args == args')
  (CILTyRef as tn)    == (CILTyRef as' tn')    = as   == as'  && tn == tn'
  (CILTyVal as tn)    == (CILTyVal as' tn')    = as   == as'  && tn == tn'
  (CILTyArr elTy)     == (CILTyArr elTy')      = elTy == elTy'
  _                   == _                     = False

%inline
CILTyObj : CILTy
CILTyObj = CILTyRef "" "object"

%inline
CILTyStr : CILTy
CILTyStr = CILTyRef "" "string"

%inline
CILTyBool : CILTy
CILTyBool = CILTyVal "" "bool"

||| A foreign CIL type.
data CIL   : CILTy -> Type where
     MkCIL : (ty : CILTy) -> CIL ty

||| A foreign descriptor.
data CILForeign =
  ||| Call the named instance method.
  CILInstance String |
  ||| Call the named instance method with the given signature.
  CILInstanceCustom String (List CILTy) CILTy |
  ||| Read the value of the named instance field.
  CILInstanceField String |
  ||| Call the named static method of the given foreign type.
  CILStatic CILTy String |
  ||| Read the value of the named static field of the given foreign type.
  CILStaticField CILTy String |
  ||| Box the integer value given as a string into the given enum type.
  CILEnumValueOf CILTy String |
  ||| Call a constructor to instantiate an object.
  CILConstructor |
  ||| Load the given runtime type.
  CILTypeOf CILTy |
  ||| Convert a function to a delegate of the given type.
  CILDelegate CILTy |
  ||| Export a function under the given name.
  CILExport String |
  ||| Export a function under its original name.
  CILDefault

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
       CIL_FnT   : CIL_FnTypes fnT -> CIL_Types (CilFn delegateTy fnT)

  data CilFn   : CILTy -> Type -> Type where
       MkCilFn : (delegateTy : CILTy) -> (fn : fnT) -> CilFn delegateTy fnT

  data CIL_FnTypes : Type -> Type where
       CIL_Fn      : CIL_Types s -> CIL_FnTypes t -> CIL_FnTypes (s -> t)
       CIL_FnIO    : CIL_Types t -> CIL_FnTypes (IO' l t)
       CIL_FnBase  : CIL_Types t -> CIL_FnTypes t

%used MkCilFn fn

FFI_CIL : FFI
FFI_CIL = MkFFI CIL_Types CILForeign String

CIL_IO : Type -> Type
CIL_IO a = IO' FFI_CIL a

||| CIL FFI.
invoke : CILForeign -> (ty : Type) ->
         {auto fty : FTy FFI_CIL [] ty} -> ty
invoke ffi ty = foreign FFI_CIL ffi ty

%inline
new : (ty : Type) ->
      {auto fty : FTy FFI_CIL [] ty} -> ty
new ty = invoke CILConstructor ty

%inline
delegate : (ty : CILTy) -> (fnT : Type) -> fnT ->
           {auto fty : FTy FFI_CIL [] (CilFn ty fnT -> CIL_IO (CilFn ty fnT))} ->
           CIL_IO (CilFn ty fnT)
delegate ty fnT fn = invoke (CILDelegate ty)
                            (CilFn ty fnT -> CIL_IO (CilFn ty fnT))
                            (MkCilFn ty fn)

%inline
corlibTy : String -> CILTy
corlibTy = CILTyRef "mscorlib"

%inline
corlibTyVal : String -> CILTy
corlibTyVal = CILTyVal "mscorlib"

%inline
corlib : String -> Type
corlib = CIL . corlibTy

Object : Type
Object = CIL CILTyObj

ObjectArray : Type
ObjectArray = CIL (CILTyArr CILTyObj)

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
  invoke (CILInstance "ToString")
         (Object -> CIL_IO String)
         (believe_me obj)

Equals : IsA Object a => a -> a -> CIL_IO Bool
Equals x y =
  invoke (CILInstance "Equals")
         (Object -> Object -> CIL_IO Bool)
         (believe_me x) (believe_me y)

namespace System.Array
  ArrayTy : CILTy
  ArrayTy = corlibTy "System.Array"

  Array : Type
  Array = CIL ArrayTy

  CreateInstance : RuntimeType -> Int -> CIL_IO Array
  CreateInstance =
    invoke (CILStatic ArrayTy "CreateInstance")
           (RuntimeType -> Int -> CIL_IO Array)

  SetValue : Array -> Object -> Int -> CIL_IO ()
  SetValue =
    invoke (CILInstance "SetValue")
           (Array -> Object -> Int -> CIL_IO ())

  partial
  fromList : List a -> CIL_IO ObjectArray
  fromList [v] = do
    array <- CreateInstance !(typeOf CILTyObj) 1
    SetValue array (believe_me v) 0
    return $ believe_me array

namespace System.Convert
  ToInt32 : IsA Object a => a -> CIL_IO Int
  ToInt32 o =
    invoke (CILStatic (corlibTy "System.Convert") "ToInt32")
           (Ptr -> CIL_IO Int)
           (believe_me o)

putStrLn : String -> CIL_IO ()
putStrLn = putStrLn'

printLn : Show a => a -> CIL_IO ()
printLn = printLn'
