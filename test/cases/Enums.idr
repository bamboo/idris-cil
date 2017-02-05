{-
1
reading
2
writing
3
writing
-}
module Main

import CIL.FFI
import CIL.Elab.Enums

%language ElabReflection

{-

  [Flags]
  enum System.IO.FileAccess {
    Read  = 1,
    Write = 2,
    ReadWrite = Read + Write
  }

-}

namespace System.IO.FileAccess

  FileAccessTy : CILTy
  FileAccessTy = corlibTyVal "System.IO.FileAccess"

  %runElab
    cilEnum FileAccess FileAccessTy Bits32
            [ cilField Read  1
            , cilField Write 2
            ]

  IsA Object FileAccess where {}

  ReadWrite :  FileAccess
  ReadWrite = Read + Write

describe : Bits32 -> String
describe 1 = "reading"
describe _ = "writing"

testEnumAsObject : FileAccess -> CIL_IO ()
testEnumAsObject e = putStrLn !(ToString e)

testFromEnum : FileAccess -> CIL_IO ()
testFromEnum e = putStrLn (describe (fromEnum e))

main : CIL_IO ()
main = do
  for_ [Read, Write, ReadWrite] $
    \e => do testEnumAsObject e
             testFromEnum e

-- Local Variables:
-- idris-load-packages: ("cil")
-- End:
