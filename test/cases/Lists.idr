{-
0
1
2
-}
module Main

length' : List a -> Nat
length' = length'' Z
  where length'' acc []      = acc
        length'' acc (x::xs) = length'' (S acc) xs

printList : List String -> IO ()
printList (x :: xs) = do putStrLn x
                         printList xs
printList []        = return ()

main : IO ()
main = printList $ map (show . length') lists
  where lists : List (List Nat)
        lists = [[1..l] | l <- [0..2]]
