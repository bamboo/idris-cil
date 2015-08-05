{-
foo
-}
module Main

main : IO ()
main = putStrLn . pack $ ['f', 'o', 'o']
