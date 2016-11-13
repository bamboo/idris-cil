{-
205
[]
True
True
36
[(1, 5), (1, 6), (2, 5), (2, 6), (11, 5), (11, 6), (11, 7), (12, 4), (12, 8), (13, 3), (13, 9), (14, 3), (14, 9), (15, 6), (16, 4), (16, 8), (17, 5), (17, 6), (17, 7), (18, 6), (21, 3), (21, 4), (21, 5), (22, 3), (22, 4), (22, 5), (23, 2), (23, 6), (25, 1), (25, 2), (25, 6), (25, 7), (35, 3), (35, 4), (36, 3), (36, 4)]
-}

import Data.HashSet

%default total

data CollidingInt = MkCollidingInt Int

Eq CollidingInt where
  (==) (MkCollidingInt i) (MkCollidingInt j) = i == j

Hash CollidingInt where
  hash (MkCollidingInt i) = assert_total (prim__zextInt_B32 (mod i 39))

Show CollidingInt where
  show (MkCollidingInt i) = show i

Cell : Type
Cell = (Int, Int)

Cells : Type
Cells = HashSet Cell

gosperGun : Cells
gosperGun = fromList
  [(1,5),  (1,6),  (2,5),  (2,6),
   (11,5), (11,6), (11,7), (12,4), (12,8),
   (13,3), (13,9), (14,3), (14,9), (15,6),
   (16,4), (16,8), (17,5), (17,6), (17,7), (18,6),
   (21,3), (21,4), (21,5), (22,3), (22,4), (22,5),
   (23,2), (23,6), (25,1), (25,2), (25,6), (25,7),
   (35,3), (36,3), (35,4), (36,4)]

for : Monad m => HashSet a -> (a -> m b) -> m (List b)
for xs f = foldM (\acc, e => (:: acc) <$> f e) [] xs

main : IO ()
main = do
  let keys = MkCollidingInt <$> [0, 5..1024]
  let set = foldl (flip insert) (the (HashSet CollidingInt) empty) keys
  printLn (length set)
  printLn (filter (not . flip member set) keys)
  printLn (all (flip member set) keys)
  printLn (all (not . flip member set) (MkCollidingInt <$> [1024, 1033..2048]))

  let cells = gosperGun
  printLn (length cells)
  ls <- for cells pure
  printLn (sort ls)

-- Local Variables:
-- idris-load-packages: ("cil")
-- End:
