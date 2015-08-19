{-
42
r
-}
module Main

systemMax : Int -> Int -> IO Int
systemMax n k =
  foreign FFI_C
    "[mscorlib]System.Math::Max"
    (Int -> Int -> IO Int)
    n k

substring : String -> Int -> Int -> IO String
substring this index count =
  foreign FFI_C
    "instance [mscorlib]System.String::Substring"
    (String -> Int -> Int -> IO String)
    this index count

main : IO ()
main = do
  systemMax 42 1 >>= printLn
  substring "idris" 2 1 >>= putStrLn
