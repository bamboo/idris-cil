{-
0.42
1.68
1.34
0.34
0.5 < 0.5 => False
0.5 <= 0.5 => True
0.5 == 0.5 => True
0.5 >= 0.5 => True
0.5 > 0.5 => False
0.5 compare 0.5 => EQ
0.5 max 0.5 => 0.5
0.5 min 0.5 => 0.5
0.5 < 1 => True
0.5 <= 1 => True
0.5 == 1 => False
0.5 >= 1 => False
0.5 > 1 => False
0.5 compare 1 => LT
0.5 max 1 => 1
0.5 min 1 => 0.5
1 < 0.5 => False
1 <= 0.5 => False
1 == 0.5 => False
1 >= 0.5 => True
1 > 0.5 => True
1 compare 0.5 => GT
1 max 0.5 => 1
1 min 0.5 => 0.5
1 < 1 => False
1 <= 1 => True
1 == 1 => True
1 >= 1 => True
1 > 1 => False
1 compare 1 => EQ
1 max 1 => 1
1 min 1 => 1
abs -1 => 1
- (-1) => 1
abs 1 => 1
- (1) => -1
-}
import CIL.FFI.Single

implementation Show Ordering where
  show EQ = "EQ"
  show LT = "LT"
  show GT = "GT"

testNum : IO ()
testNum =
  for_ [(*), (/), (+), (-)] $ \op =>
    printLn $ single 0.84 `op` single 0.5

testOrd : IO ()
testOrd = traverse_ putStrLn (test singles singles)
  where
    singles = [ single 0.5, single 1.0 ]

    op : Show r => String -> (Single -> Single -> r) -> (Single -> Single -> String)
    op name o = \x, y => show x ++ " " ++  name ++ " " ++ show y ++ " => " ++ show (x `o` y)

    operators : List (Single -> Single -> String)
    operators = [ op "<" (<), op "<=" (<=), op "==" (==)
                , op ">=" (>=), op ">" (>), op "compare" compare
                , op "max" max, op "min" min ]

    test : List Single -> List Single -> List String
    test xs ys = do
      x <- xs
      y <- ys
      map (\o => x `o` y) operators

testNeg : IO ()
testNeg =
  for_ [ single (-1), single 1 ] $ \x => do
    putStrLn $ "abs " ++ show x ++ " => " ++ show (abs x)
    putStrLn $ "- (" ++ show x ++ ") => " ++ show (-x)

main : IO ()
main = do
  testNum
  testOrd
  testNeg

-- Local Variables:
-- idris-load-packages: ("cil")
-- End:
