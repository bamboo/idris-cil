module CIL.FFI.Array

import public CIL.FFI
import Data.Vect

%default total
%access public export

IsA Object (TypedArray cilTy elTy) where {}
IsA Array (TypedArray cilTy elTy) where {}

total
interp : CILTy -> Type
interp (CILTyVal "" "int") = Int
interp (CILTyVal "" "char") = Char
interp ty = CIL ty

%inline
TypedArrayOf : (elTy : CILTy) -> Type
TypedArrayOf elTy = TypedArray (CILTyArr elTy) (interp elTy)

%inline
Int32Array : Type
Int32Array = TypedArrayOf CILTyInt32

%inline
CharArray : Type
CharArray = TypedArrayOf CILTyChar

%inline
length : TypedArray cilTy elT
      -> {auto fty : FTy FFI_CIL [] (TypedArray cilTy elT -> CIL_IO Int)}
      -> CIL_IO Int
length {cilTy} {elT} a = invoke (CILInstance "get_Length")
                                (TypedArray cilTy elT -> CIL_IO Int)
                                a

%inline
get' : (elTy : CILTy)
    -> TypedArrayOf elTy
    -> (index : Int)
    -> {auto fty : FTy FFI_CIL [] (TypedArrayOf elTy -> Int -> CIL_IO (interp elTy))}
    -> CIL_IO (interp elTy)
get' elTy a i = invoke (CILInstance "get_Item")
                       (TypedArrayOf elTy -> Int -> CIL_IO (interp elTy))
                       a i

%inline
set : TypedArray cilTy elT
   -> (index : Int)
   -> (element : elT)
   -> {auto fty : FTy FFI_CIL [] (TypedArray cilTy elT -> Int -> elT -> CIL_IO ())}
   -> CIL_IO ()
set {cilTy} {elT} a i e = invoke (CILInstance "set_Item")
                                 (TypedArray cilTy elT -> Int -> elT -> CIL_IO ())
                                 a i e

%inline
newArrayOf : (elTy : CILTy) -> Int -> CIL_IO (TypedArrayOf elTy)
newArrayOf elTy n = new (Int -> CIL_IO (TypedArrayOf elTy)) n

%inline
arrayOf : (elTy : CILTy)
       -> Vect n (interp elTy)
       -> {auto fty : FTy FFI_CIL [] (TypedArrayOf elTy -> Int -> (interp elTy) -> CIL_IO ())}
       -> CIL_IO (TypedArrayOf elTy)
arrayOf {n} elTy v = do
  array <- newArrayOf elTy (cast n)
  for_ (zip [0..the Int (cast n)] (toList v)) $ \pair =>
      set array (fst pair) (snd pair)
  pure array
