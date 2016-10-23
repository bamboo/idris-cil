module CIL.FFI.Array

import public CIL.FFI
import Data.Vect

%default total
%access public export

IsA Object (TypedArray cilTy elTy) where {}
IsA Array (TypedArray cilTy elTy) where {}

total
interpCILTy : CILTy -> Type
interpCILTy (CILTyVal "" "int") = Int
interpCILTy (CILTyVal "" "char") = Char
interpCILTy (CILTyVal "" "bool") = Bool
interpCILTy (CILTyRef "" "string") = String
interpCILTy ty = CIL ty

%inline
TypedArrayOf : (elTy : CILTy) -> Type
TypedArrayOf elTy = TypedArray (CILTyArr elTy) (interpCILTy elTy)

%inline
Int32Array : Type
Int32Array = TypedArrayOf CILTyInt32

%inline
ObjectArray : Type
ObjectArray = TypedArrayOf CILTyObj

%inline
StringArray : Type
StringArray = TypedArrayOf CILTyStr

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
get : TypedArray cilTy elT
   -> (index : Int)
   -> {auto fty : FTy FFI_CIL [] (TypedArray cilTy elT -> Int -> CIL_IO elT)}
   -> CIL_IO elT
get {cilTy} {elT} a i = invoke (CILInstance "get_Item")
                               (TypedArray cilTy elT -> Int -> CIL_IO elT)
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
copyInto : TypedArray cilTy elT
        -> Int
        -> Vect n elT
        -> {auto fty : FTy FFI_CIL [] (TypedArray cilTy elT -> Int -> elT -> CIL_IO ())}
        -> CIL_IO ()
copyInto array i (x :: xs) = set array i x *> copyInto array (i + 1) xs
copyInto _     _ []        = pure ()

%inline
arrayOf : (elTy : CILTy)
       -> Vect n (interpCILTy elTy)
       -> {auto fty : FTy FFI_CIL [] (TypedArrayOf elTy -> Int -> (interpCILTy elTy) -> CIL_IO ())}
       -> CIL_IO (TypedArrayOf elTy)
arrayOf {n} elTy xs = do
  array <- newArrayOf elTy (cast n)
  copyInto array 0 xs
  pure array

%inline
forEach_ : TypedArray cilTy elT
        -> (elT -> CIL_IO ())
        -> {auto fty : FTy FFI_CIL [] (TypedArray cilTy elT -> Int -> CIL_IO elT)}
        -> CIL_IO ()
forEach_ array f = loop 0 (cast !(length array)) (\i => get array i >>= f)
  where
    loop : Int -> Nat -> (Int -> CIL_IO ()) -> CIL_IO ()
    loop i (S n) f = f i *> loop (i + 1) n f
    loop _ Z     _ = pure ()
