{-
null
is Nothing
is not Just
not null
is not Nothing
is Just
==
/=
==
Just 42
Nothing
-}

import CIL.FFI
import CIL.System.Reflection

putMaybeString : Maybe String -> CIL_IO ()
putMaybeString s =
  putStrLn (maybe "null" id s)

putIsNothing : Maybe String -> CIL_IO ()
putIsNothing Nothing = putStrLn "is Nothing"
putIsNothing _       = putStrLn "is not Nothing"

putIsJust : Maybe String -> CIL_IO ()
putIsJust (Just _) = putStrLn "is Just"
putIsJust _        = putStrLn "is not Just"

TheExportsTy : CILTy
TheExportsTy = CILTyRef "" "TheExports"

%inline
invokeMaybeString : String -> Maybe String -> CIL_IO ()
invokeMaybeString fn s =
  invokeStatic TheExportsTy fn (Maybe String -> CIL_IO ()) s

testFFI : Maybe String -> CIL_IO ()
testFFI s = do
  invokeMaybeString "putMaybeString" s
  invokeMaybeString "putIsNothing" s
  invokeMaybeString "putIsJust" s

valueOf : Bool -> Maybe String
valueOf False = Nothing
valueOf True  = Just "True"

testMaybeEq : Bool -> Maybe String -> CIL_IO ()
testMaybeEq b m = putStrLn (if valueOf b == m then "==" else "/=")

maybeInt : Bool -> Maybe Int
maybeInt True  = Just 42
maybeInt False = Nothing

testMaybeInt : Bool -> CIL_IO ()
testMaybeInt b = putStrLn $ show (maybeInt b)

%inline
invokeTestMaybeInt : Bool -> CIL_IO ()
invokeTestMaybeInt =
  invokeStatic TheExportsTy "testMaybeInt" (Bool -> CIL_IO ())

main : CIL_IO ()
main = do
  testFFI Nothing
  testFFI (Just "not null")
  testMaybeEq False Nothing
  testMaybeEq False (Just "True")
  testMaybeEq True  (Just "True")
  invokeTestMaybeInt True
  invokeTestMaybeInt False

exports : FFI_Export FFI_CIL "TheExports" []
exports =
  Fun putMaybeString CILDefault $
  Fun putIsNothing CILDefault $
  Fun putIsJust CILDefault $
  Fun testMaybeInt CILDefault $
  End

-- Local Variables:
-- idris-load-packages: ("cil")
-- End:
