{-
foo
bar
baz
-}
module Main

main : IO ()
main = do
  putStrLn . pack $ ['f', 'o', 'o']
  for_ (words "bar baz ") putStrLn
