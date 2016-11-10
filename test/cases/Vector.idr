{-
True
0
False
False
4
42
42
True
False
[33, 42, 42, 42, 42]
[42, 42, 33, 42, 42]
[42, 42, 42, 42, 33]
[33, 42, 42, 42]
[42, 42, 33, 42]
[42, 42, 42, 33]
False
True
-}

import Data.Vector

main : IO ()
main = do
  let e = the (Vector Int) empty
  printLn (null e)
  printLn (length e)
  printLn (elem 42 e)
  let a = replicate 4 (the Int 42)
  printLn (null a)
  printLn (length a)
  printLn (a !! 0)
  printLn (a !! 3)
  printLn (elem 42 a)
  printLn (elem 33 a)
  printLn (unsafeInsertAt 0 33 a)
  printLn (unsafeInsertAt 2 33 a)
  printLn (unsafeInsertAt (length a) 33 a)
  printLn (unsafeReplaceAt 0 33 a)
  printLn (unsafeReplaceAt 2 33 a)
  printLn (maybe a (\index => unsafeReplaceAt index 33 a) (lastIndex a))
  printLn (elem 33 a)
  printLn (elem 33 (singleton 33))

-- Local Variables:
-- idris-load-packages: ("cil")
-- End:
