{-
42
-}
module Main

systemMax : Int -> Int -> IO Int
systemMax n k = foreign FFI_C "[mscorlib]System.Math::Max" (Int -> Int -> IO Int) n k

main : IO ()
main = systemMax 42 1 >>= printLn
