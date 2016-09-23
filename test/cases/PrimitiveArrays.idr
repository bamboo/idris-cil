{-
System.Int32[]
17
25
42
-}
import CIL.FFI.Array
import Data.Vect

infixl 5 >=>

(>=>) : Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) f g x = f x >>= g

%inline
getI32 : Int32Array -> Int -> CIL_IO Int
getI32 a i = get' CILTyInt32 a i

main : CIL_IO ()
main = do
  array <- arrayOf CILTyInt32 [17, 25]
  putStrLn !(ToString array)
  let a = getI32 array
  for_ [0..!(length array) - 1] $
    a >=> printLn
  printLn (!(a 0) + !(a 1))
