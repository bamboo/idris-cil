{-
foo
bar
baz
'q'
'u'
'x'
-}
module Main

main : IO ()
main = do
  putStrLn . pack $ ['f', 'o', 'o']
  for_ (words "bar baz ") putStrLn
  for_ (unpack "qux") (putStrLn . show)
