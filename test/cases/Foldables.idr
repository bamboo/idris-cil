{-
55
-}
module Main

main : IO ()
main = printLn $ foldl (+) 0 [1..10]
