{-
False
foobar
f(o)(o)bar
True
foofoo
f(o)(o)f(o)(o)
-}
module Main

test : String -> String -> IO ()
test s1 s2 = do printLn $ s1 == s2
                putStrLn $ s1 ++ s2
                putStrLn $ concatMap (hi 'o') $ unpack (s1 ++ s2)
  where hi : Char -> Char -> String
        hi c d = pack $
                  if c == d
                    then ['(', c, ')']
                    else [d]

main : IO ()
main = do
  test "foo" "bar"
  test "foo" "foo"
