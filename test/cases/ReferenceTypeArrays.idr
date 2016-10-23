{-
foo
bar
baz
qux
quux
-}

import CIL.FFI
import CIL.FFI.Array
import Data.Vect

ToCharArray : String -> CIL_IO CharArray
ToCharArray =
  invoke (CILInstance "ToCharArray")
         (String -> CIL_IO CharArray)

Split : String -> CharArray -> CIL_IO StringArray
Split =
  invoke (CILInstance "Split")
         (String -> CharArray -> CIL_IO StringArray)

putAll : StringArray -> CIL_IO ()
putAll ss = forEach_ ss putStrLn

main : CIL_IO ()
main = do
  Split "foo,bar baz" !(ToCharArray ", ") >>= putAll
  arrayOf CILTyStr ["qux", "quux"] >>= putAll

-- Local Variables:
-- idris-load-packages: ("cil")
-- End:
