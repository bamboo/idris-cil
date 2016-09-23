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

main : CIL_IO ()
main = do
  array <- arrayFrom [17, 25]
  putStrLn !(ToString array)
  let a = get array
  for_ [0..!(length array) - 1] $
    a >=> printLn
  printLn (!(a 0) + !(a 1))
