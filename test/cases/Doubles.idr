{-
0.42
1.68
1.34
0.34
42
0
-}
module Main

testNum : IO ()
testNum =
  traverse_
    (\op => printLn $ 0.84 `op` 0.5)
    [(*), (/), (+), (-)]

testStr : IO ()
testStr =
  for_ ["42.0", "not a double"] $ \s =>
    printLn $ the Double (cast s)

main : IO ()
main = do
  testNum
  testStr
