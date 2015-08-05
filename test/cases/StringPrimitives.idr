{-
False
foobar
True
foofoo
-}
module Main

test : String -> String -> IO ()
test s1 s2 = do printLn $ s1 == s2
                putStrLn $ s1 ++ s2

main : IO ()
main = do
  test "foo" "bar"
  test "foo" "foo"
