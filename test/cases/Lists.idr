{-
2
1
2
-}
module Main

length' : List a -> Nat
length' = length'' Z
  where length'' acc []      = acc
        length'' acc (x::xs) = length'' (S acc) xs

printList : List String -> IO ()
printList (x :: xs) = putStrLn x *> printList xs
printList []        = pure ()

main : IO ()
main = printList $ (show . length') <$> lists
-- TODO: printList $ map show $ sort [3, 4, 1]
  where lists : List (List Nat)
        lists = [[1..l] | l <- [0..2]]
