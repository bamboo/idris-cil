{-
a[0] = {foo: bar}
a[1] = {baz: qux}
-}
import CIL.FFI.Array
import Data.Vect

DictionaryEntryTy : CILTy
DictionaryEntryTy = corlibTyVal "System.Collections.DictionaryEntry"

||| System.Collections.DictionaryEntry value type.
DictionaryEntry : Type
DictionaryEntry = CIL DictionaryEntryTy

newDictionaryEntry : key -> value -> CIL_IO DictionaryEntry
newDictionaryEntry key value = new (Object -> Object -> CIL_IO DictionaryEntry)
                                   (believe_me key)
                                   (believe_me value)

Key : DictionaryEntry -> CIL_IO Object
Key = invoke (CILInstance "get_Key") (DictionaryEntry -> CIL_IO Object)

Value : DictionaryEntry -> CIL_IO Object
Value = invoke (CILInstance "get_Value") (DictionaryEntry -> CIL_IO Object)

main : CIL_IO ()
main = do
  a <- arrayOf DictionaryEntryTy [ !(newDictionaryEntry "foo" "bar")
                                 , !(newDictionaryEntry "baz" "qux") ]
  for_ [0..1] $ \i => do
    e <- get a i
    k <- Key e >>= ToString
    v <- Value e >>= ToString
    putStrLn ("a[" ++ show i ++ "] = {" ++ k ++ ": " ++ v ++ "}")

-- Local Variables:
-- idris-load-packages: ("cil")
-- End:
