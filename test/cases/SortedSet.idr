{-
[(0, 1), (1, 0), (1, 1), (2, 2)]
-}
module Main

import Data.SortedSet

%hide merge

Cell : Type
Cell = (Int, Int)

Nil : Ord k => SortedSet k
Nil = SortedSet.empty

(::) : Ord k => k -> SortedSet k -> SortedSet k
(::) = insert

Cells : Type
Cells = SortedSet Cell

Show Cells where
  show = show . SortedSet.toList

merge : Cells -> Cells -> Cells
merge xs ys = foldl (flip insert) xs (SortedSet.toList ys)

main : IO ()
main = printLn $ [(0, 1), (1, 0), (2, 2)] `merge` [(1, 0), (0, 1), (1, 1)]
