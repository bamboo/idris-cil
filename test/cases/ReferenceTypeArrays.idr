{-
foo
bar
baz
-}

import CIL.FFI
import CIL.FFI.Array

ToCharArray : String -> CIL_IO CharArray
ToCharArray =
  invoke (CILInstance "ToCharArray")
         (String -> CIL_IO CharArray)

Split : String -> CharArray -> CIL_IO StringArray
Split =
  invoke (CILInstance "Split")
         (String -> CharArray -> CIL_IO StringArray)

main : CIL_IO ()
main = do
  ss <- Split "foo,bar baz" !(ToCharArray ", ")
  forEach_ CILTyStr ss $
    putStrLn
