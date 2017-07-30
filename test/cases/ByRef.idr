{-
The String
-}
module Main

import CIL.FFI
import CIL.FFI.Array

%inline
ByRef : CILTy -> Type
ByRef refTy = CILRef refTy (interpCILTy refTy)

namespace ByRef

  %inline
  ByRefTy : CILTy -> Type
  ByRefTy refTy = CIL $ CILTyGen (CILTyRef "" "ByRef") [refTy]

  %inline
  new : (refTy : CILTy)
     -> {auto fty : FTy FFI_CIL [] (interpCILTy refTy -> CIL_IO (ByRefTy refTy))}
     -> interpCILTy refTy
     -> CIL_IO (ByRef refTy)
  new refTy value = do
    ref <- CIL.FFI.new (interpCILTy refTy -> CIL_IO (ByRefTy refTy)) value
    pure (believe_me ref)

  %inline
  get : CILRef refTy refT
     -> {auto fty : FTy FFI_CIL [] (CILRef refTy refT -> CIL_IO refT)}
     -> CIL_IO refT
  get {refTy} {refT} a =
    invokeInstance "get" (CILRef refTy refT -> CIL_IO refT) a

StringTy : CILTy
StringTy = CILTyStr

FixtureTy : CILTy
FixtureTy = CILTyRef "" "Fixture"

passStringByRef : ByRef StringTy -> CIL_IO ()
passStringByRef = invokeStatic FixtureTy "PassStringByRef" (ByRef StringTy -> CIL_IO ())

main : CIL_IO ()
main = do
  ref <- ByRef.new StringTy "Not the string"
  passStringByRef ref
  get ref >>= putStrLn

-- Local Variables:
-- idris-load-packages: ("cil")
-- End:
