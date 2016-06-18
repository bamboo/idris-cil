{-
0.42
1.68
1.34
0.34
-}
module Main

main : IO ()
main =
  traverse_
    (\op => printLn $ 0.84 `op` 0.5)
    [(*), (/), (+), (-)]
