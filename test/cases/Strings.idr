{-
False
foobar
oof
rab
True
foofoo
oof
oof
-}
module Main

test : String -> String -> IO ()
test s1 s2 = do printLn $ s1 == s2
                putStrLn $ s1 ++ s2
                for_ [s1, s2] $ putStrLn . reverse

main : IO ()
main = do
  test "foo" "bar"
  test "foo" "foo"
