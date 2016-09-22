{-
foo
bar
-}
import CIL.FFI

||| System.Collections.DictionaryEntry value type.
DictionaryEntry : Type
DictionaryEntry = CIL $ corlibTyVal "System.Collections.DictionaryEntry"

IsA Object DictionaryEntry where {}

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
  e <- newDictionaryEntry "foo" "bar"
  for_ [Key, Value] $ \p =>
    putStrLn !(p e >>= ToString)
