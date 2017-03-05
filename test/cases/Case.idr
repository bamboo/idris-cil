{-
'A'
'_'
'C'
True
False
True
True
False
True
-}
module Main

data D = A | B | C

adt : D -> IO Bool
adt A = pure True
adt C = pure True
adt _ = pure False

int : Int -> IO Bool
int 0          = pure True
int 2147483647 = pure True
int _          = pure False

char : Char -> IO Char
char 'C' = pure 'C'
char 'B' = pure 'B'
char 'A' = pure 'A'
char _   = pure '_'

printMap : (Traversable t, Show b) => (a -> IO b) -> t a -> IO ()
printMap f xs = for_ xs (\x => f x >>= printLn)

main : IO ()
main = do
  printMap char ['A', 'D', 'C']
  printMap int  [0, 1, 2147483647]
  printMap adt  [A, B, C]
