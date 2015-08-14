{-
<
=
>
-}

instance Show Ordering where
  show LT = "<"
  show GT = ">"
  show EQ = "="

test : Ord a => a -> a -> IO ()
test x y = printLn $ compare x y

l : List (Int, Int)
l = [(33, 42), (42, 42), (42, 33)]

main : IO ()
main = traverse_ (uncurry test) l
