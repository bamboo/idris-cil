{-
0
1
0
-}
module Main

testInt : String -> IO ()
testInt s = do
  let x : Int = cast s
  putStrLn $ cast x

testInteger : String -> IO ()
testInteger s = do
  let x : Integer = cast s
  putStrLn $ cast x

main : IO ()
main = do
  traverse_ testInt ["0", "1", "abc"]
  --traverse_ testInteger ["0", "1", "abc"]
