module IRTS.Cil.OptimizeLocals (optimizeLocals) where

import IRTS.Cil.Types

import Data.DList hiding (map)
import Data.Maybe

import Language.Cil


optimizeLocals :: Int -> [Instruction] -> DList Instruction
optimizeLocals localCount = fromList . (locals ++) . optimizedInstructions
  where
    optimizedInstructions = map shorten . removeRedundantSequences . map normalize
    locals = [localsInit (local <$> [0..(localCount - 1)]) | localCount > 0]
    local  = Local Object . ("l" ++) . show


removeRedundantSequences :: [Instruction] -> [Instruction]
removeRedundantSequences (OpCode (Ldloc i) : OpCode (Stloc k) : xs)
  | i == k = removeRedundantSequences xs
removeRedundantSequences (OpCode (Stloc i) : OpCode (Ldloc k) : xs@(Label{} : OpCode Ret : _))
  | i == k = removeRedundantSequences xs
removeRedundantSequences (OpCode (Stloc i) : OpCode (Ldloc k) : xs@(OpCode Ret : _))
  | i == k = removeRedundantSequences xs
removeRedundantSequences (x : xs) = x : removeRedundantSequences xs
removeRedundantSequences [] = []


normalize :: Instruction -> Instruction
normalize (OpCode Stloc_0) = OpCode (Stloc 0)
normalize (OpCode Stloc_1) = OpCode (Stloc 1)
normalize (OpCode Stloc_2) = OpCode (Stloc 2)
normalize (OpCode Stloc_3) = OpCode (Stloc 3)
normalize (OpCode Ldloc_0) = OpCode (Ldloc 0)
normalize (OpCode Ldloc_1) = OpCode (Ldloc 1)
normalize (OpCode Ldloc_2) = OpCode (Ldloc 2)
normalize (OpCode Ldloc_3) = OpCode (Ldloc 3)
normalize (OpCode Ldarg_0) = OpCode (Ldarg 0)
normalize (OpCode Ldarg_1) = OpCode (Ldarg 1)
normalize (OpCode Ldarg_2) = OpCode (Ldarg 2)
normalize (OpCode Ldarg_3) = OpCode (Ldarg 3)
normalize other = other


shorten :: Instruction -> Instruction
shorten (OpCode (Stloc 0)) = OpCode Stloc_0
shorten (OpCode (Stloc 1)) = OpCode Stloc_1
shorten (OpCode (Stloc 2)) = OpCode Stloc_2
shorten (OpCode (Stloc 3)) = OpCode Stloc_3
shorten (OpCode (Ldloc 0)) = OpCode Ldloc_0
shorten (OpCode (Ldloc 1)) = OpCode Ldloc_1
shorten (OpCode (Ldloc 2)) = OpCode Ldloc_2
shorten (OpCode (Ldloc 3)) = OpCode Ldloc_3
shorten (OpCode (Ldarg 0)) = OpCode Ldarg_0
shorten (OpCode (Ldarg 1)) = OpCode Ldarg_1
shorten (OpCode (Ldarg 2)) = OpCode Ldarg_2
shorten (OpCode (Ldarg 3)) = OpCode Ldarg_3
shorten other = other
