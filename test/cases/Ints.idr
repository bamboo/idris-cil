{-
False
True
False
-}

test : Int -> Int -> IO ()
test x y = printLn $ x == y

main : IO ()
main = do
  test 33 42
  test 42 42
  test 42 33
