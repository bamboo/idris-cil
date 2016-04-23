{-
A 42
-}

module Main

data V a = A a

Show a => Show (V a) where
  show (A a) = "A " ++ show a

main : IO ()
main = printLn $ A 42
