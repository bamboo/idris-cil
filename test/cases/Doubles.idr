{-
0.42
-}
module Main

import CIL.FFI

-- Using a foreign function to hide the result from
-- Idris
Max : Double -> Double -> CIL_IO Double
Max =
  invoke
    (CILStatic (CILTyRef "mscorlib" "System.Math") "Max")
    (Double -> Double -> CIL_IO Double)

multiplication : CIL_IO ()
multiplication = printLn (0.84 * !(Max 0.1 0.5))

main : CIL_IO ()
main = do
  multiplication
