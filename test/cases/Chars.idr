{-
foo
bar
baz
'\t'
'q'
'u'
'x'
'\r'
'\n'
-}
module Main

main : IO ()
main = do
  putStrLn . pack $ ['f', 'o', 'o']
  for_ (words "bar baz ") putStrLn
  for_ (unpack "\tqux\r\n") (putStrLn . show)
