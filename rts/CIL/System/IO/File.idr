module CIL.System.IO.File

import CIL.FFI

%access public export

namespace System.IO.File

  %inline
  FileTy : CILTy
  FileTy = CILTyRef "System.IO.FileSystem" "System.IO.File"

  %inline
  ReadAllText : String -> CIL_IO String
  ReadAllText = invokeStatic FileTy "ReadAllText" (String -> CIL_IO String)

  %inline
  WriteAllText : String -> String -> CIL_IO ()
  WriteAllText = invokeStatic FileTy "WriteAllText" (String -> String -> CIL_IO ())
