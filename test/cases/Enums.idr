{-
Read
reading
Write
writing
ReadWrite
writing
-}
module Main

import CIL.FFI

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

  FileAccess : Type
  FileAccess = CIL FileAccessTy

  IsA Object FileAccess where {}

  Read : CIL_IO FileAccess
  Read = invoke (CILEnumValueOf FileAccessTy "1")
                (CIL_IO FileAccess)

  Write : CIL_IO FileAccess
  Write = invoke (CILEnumValueOf FileAccessTy "2")
                 (CIL_IO FileAccess)

  ReadWrite : CIL_IO FileAccess
  ReadWrite = invoke (CILEnumValueOf FileAccessTy "3")
                     (CIL_IO FileAccess)

namespace System.IO.TFileAccess

  ||| ADT version of `System.IO.FileAccess`.
  data TFileAccess = Read
                   | Write
                   | ReadWrite

  fromEnum : FileAccess -> CIL_IO TFileAccess
  fromEnum e = do
    i32 <- ToInt32 e
    pure $
      case i32 of
        1 => Read
        2 => Write
        _ => ReadWrite

describe : TFileAccess -> String
describe Read = "reading"
describe _    = "writing"

testEnumAsObject : FileAccess -> CIL_IO ()
testEnumAsObject e = putStrLn !(ToString e)

testFromEnum : FileAccess -> CIL_IO ()
testFromEnum e = putStrLn (describe !(fromEnum e))

main : CIL_IO ()
main = do
  for_ [!Read, !Write, !ReadWrite] $
    \e => do testEnumAsObject e
             testFromEnum e
