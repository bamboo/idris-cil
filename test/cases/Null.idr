{-
null
not null
-}

import CIL.FFI

putNullableStrLn : Nullable String -> IO ()
putNullableStrLn =
  putStrLn . nullable "null" id

main : IO ()
main = do
  putNullableStrLn null
  putNullableStrLn (asNullable "not null")

-- Local Variables:
-- idris-load-packages: ("cil")
-- End:
