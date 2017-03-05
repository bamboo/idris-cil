{-# LANGUAGE StandaloneDeriving #-}

module IRTS.Cil.CaseDispatch where

data DispatchStrategy a
  = JumpTable    [(Int, JumpTableEntry a)]
  | LinearSearch [(Int, a)]
  | SearchTree   Int (DispatchStrategy a) (DispatchStrategy a) -- TODO

deriving instance Show a => Show (DispatchStrategy a)
deriving instance Eq a => Eq (DispatchStrategy a)

data JumpTableEntry a
  = Entry a
  | DefaultEntry

deriving instance Show a => Show (JumpTableEntry a)
deriving instance Eq a => Eq (JumpTableEntry a)

dispatchStrategyFor :: [(Int, a)] -> DispatchStrategy a
dispatchStrategyFor alts
  | length alts > 2 && isDense (fst <$> alts) = JumpTable (jumpTableEntriesFor alts)
  | otherwise                                 = LinearSearch alts

jumpTableEntriesFor :: [(Int, a)] -> [(Int, JumpTableEntry a)]
jumpTableEntriesFor = entries
  where
    entries (x@(i, _) : y@(j, _) : ys) = entry x : (defaultEntry <$> [i + 1 .. j - 1]) ++ entries (y : ys)
    entries xs = entry <$> xs
    entry = fmap Entry
    defaultEntry i = (i, DefaultEntry)

isDense :: [Int] -> Bool
isDense (x : y : ys) = (x < y) && (y - x <= 2) && isDense (y : ys)
isDense _            = True
