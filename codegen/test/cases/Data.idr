{-
[1, 42, 51]
True
False
-}
module Main

data T a = L | B a (T a) (T a)

toList : T a -> List a
toList L = []
toList (B a l r) = Main.toList l ++ [a] ++ Main.toList r

contains : Ord a => T a -> a -> Bool
contains L _ = False
contains (B a l r) x =
  case (compare x a) of
    EQ => True
    LT => contains l x
    GT => contains r x

main : IO ()
main = do printLn $ Main.toList tree
          printLn $ contains tree 1
          printLn $ contains tree 52
  where tree : T Nat
        tree = (B 42 (B 1 L L) (B 51 L L))
