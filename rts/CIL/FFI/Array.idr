module CIL.FFI.Array

import public CIL.FFI
import Data.Vect

%default total
%access public export

Int32Array : Type
Int32Array = TypedArray (CILTyArr CILTyInt32) Int

IsA Object (TypedArray cilTy elTy) where {}
IsA Array (TypedArray cilTy elTy) where {}

get : Int32Array -> Int -> CIL_IO Int
get = invoke (CILInstance "get_Item") (Int32Array -> Int -> CIL_IO Int)

set : Int32Array -> (index : Int) -> (value : Int) -> CIL_IO ()
set = invoke (CILInstance "set_Item") (Int32Array -> Int -> Int -> CIL_IO ())

length : Int32Array -> CIL_IO Int
length = invoke (CILInstance "get_Length") (Int32Array -> CIL_IO Int)

intoArray : Int32Array -> Vect n Int -> CIL_IO ()
intoArray {n} a v =
  for_ (zip [0..the Int (cast n)] (toList v)) $ uncurry (set a)

arrayFrom : Vect n Int -> CIL_IO Int32Array
arrayFrom {n} v = do
  array <- new (Int -> CIL_IO Int32Array) (cast n)
  intoArray array v
  pure array
