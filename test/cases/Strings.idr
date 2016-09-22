{-
False
foobar
oof
rab
True
foofoo
oof
oof
(Hello)
()
( Idris)
(s)
(s!)
-}
module Main

test : String -> String -> IO ()
test s1 s2 = do printLn $ s1 == s2
                putStrLn $ s1 ++ s2
                for_ [s1, s2] $ putStrLn . reverse

testSubstr : (Nat, Nat) -> IO ()
testSubstr (index, length) = putStrLn $ "(" ++ (substr index length "Hello, Idris!") ++ ")"

main : IO ()
main = do
  test "foo" "bar"
  test "foo" "foo"
  traverse_ testSubstr [(0, 5), (20, 5), (6, 6), (11, 1), (11, 5)]
