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
'I'
'd'
'r'
'i'
's'
"foo" < "foo" => False
"foo" <= "foo" => True
"foo" == "foo" => True
"foo" >= "foo" => True
"foo" > "foo" => False
"foo" max "foo" => "foo"
"foo" min "foo" => "foo"
"foo" < "bar" => False
"foo" <= "bar" => False
"foo" == "bar" => False
"foo" >= "bar" => True
"foo" > "bar" => True
"foo" max "bar" => "foo"
"foo" min "bar" => "bar"
"bar" < "foo" => True
"bar" <= "foo" => True
"bar" == "foo" => False
"bar" >= "foo" => False
"bar" > "foo" => False
"bar" max "foo" => "foo"
"bar" min "foo" => "bar"
"bar" < "bar" => False
"bar" <= "bar" => True
"bar" == "bar" => True
"bar" >= "bar" => True
"bar" > "bar" => False
"bar" max "bar" => "bar"
"bar" min "bar" => "bar"
-}
module Main

test : String -> String -> IO ()
test s1 s2 = do printLn $ s1 == s2
                putStrLn $ s1 ++ s2
                for_ [s1, s2] $ putStrLn . reverse

testSubstr : (Nat, Nat) -> IO ()
testSubstr (index, length) = putStrLn $ "(" ++ (substr index length "Hello, Idris!") ++ ")"

testOrd : IO ()
testOrd = traverse_ putStrLn (test strings strings)
  where
    strings = [ "foo", "bar" ]

    op : Show r => String -> (String -> String -> r) -> (String -> String -> String)
    op name o = \x, y => show x ++ " " ++  name ++ " " ++ show y ++ " => " ++ show (x `o` y)

    operators : List (String -> String -> String)
    operators = [ op "<" (<), op "<=" (<=), op "==" (==)
                , op ">=" (>=), op ">" (>)
                , op "max" max, op "min" min ]

    test : List String -> List String -> List String
    test xs ys = do
      x <- xs
      y <- ys
      map (\o => x `o` y) operators


main : IO ()
main = do
  test "foo" "bar"
  test "foo" "foo"
  traverse_ testSubstr [(0, 5), (20, 5), (6, 6), (11, 1), (11, 5)]
  traverse_ (putStrLn . show . strIndex "Idris") [0..4]
  testOrd
