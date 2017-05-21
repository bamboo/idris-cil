module CIL.System.Environment

import CIL.FFI
import CIL.FFI.Array

%access public export

namespace System.Environment

  EnvironmentTy : CILTy
  EnvironmentTy = corlibTy "System.Environment"

  GetCommandLineArgs : CIL_IO StringArray
  GetCommandLineArgs = invokeStatic EnvironmentTy "GetCommandLineArgs" (CIL_IO StringArray)

getArgs : CIL_IO (List String)
getArgs = do
  args <- GetCommandLineArgs
  toList args
