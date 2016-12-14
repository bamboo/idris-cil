module IRTS.Cil.OptimizeLocals (optimizeLocals) where

import           IRTS.Cil.Types

import           Data.DList hiding (map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid

import           Language.Cil

data InferredType
  = Impossible
  | Possibly PrimitiveType
  deriving Show

type InferredTypes = M.Map Int InferredType

type LocalTypes = M.Map Int PrimitiveType

optimizeLocals :: Int -> [Instruction] -> DList Instruction
optimizeLocals lc is =
  let inferredTypes = inferredTypesFor instructions
      localTypes = M.mapMaybe inferredType inferredTypes
  in if null localTypes
       then fromList (locals localTypes <> map shorten instructions)
       else fromList (comment (show localTypes) : optimized localTypes)
  where
    inferredType Impossible = Nothing
    inferredType (Possibly t) = Just t
    optimized types = locals types <> map shorten (optimize types instructions)
    instructions = removeRedundantSequences (map normalize is)
    locals types = [localsInit $ local types <$> [0..(lc - 1)] | lc > 0]
    local types i = Local (M.findWithDefault Object i types) ("l" <> show i)


-- Infer types by looking at how local variables are actually used,
-- e.g. by taking (Ldloc id) followed by (Unbox_any type) sequences
-- into account.
inferredTypesFor :: [Instruction] -> InferredTypes
inferredTypesFor is = ensureCompatibleAssignmentsOf inferredTypes
  where
    inferredTypes = foldl unify M.empty ldlocUnboxPairs

    unify :: InferredTypes -> (Int, PrimitiveType) -> InferredTypes
    unify types (index, t) = M.alter (unifyWith t) index types

    unifyWith :: PrimitiveType -> Maybe InferredType -> Maybe InferredType
    unifyWith t Nothing = Just (Possibly t)
    unifyWith t1 inferredType@(Just (Possibly t2)) | t1 == t2 = inferredType
    unifyWith _ _ = Just Impossible

    ldlocUnboxPairs = mapMaybe ldlocFollowedByUnbox instructionPairs
    instructionPairs = zip is (Prelude.tail is)
    ldlocFollowedByUnbox (OpCode (Ldloc index), OpCode (Unbox_any t)) = Just (index, t)
    ldlocFollowedByUnbox (_, _) = Nothing

    ensureCompatibleAssignmentsOf types = foldl ensureCompatibleAssignment types instructionPairs

    ensureCompatibleAssignment :: InferredTypes -> (Instruction, Instruction) -> InferredTypes
    -- assume copying from locals or arguments doesn't imply incompatibility
    ensureCompatibleAssignment types (OpCode Ldarg{}, OpCode (Stloc index)) = types
    ensureCompatibleAssignment types (OpCode Ldloc{}, OpCode (Stloc index)) = types
    -- but copying from any other source might
    ensureCompatibleAssignment types (OpCode (Box t), OpCode (Stloc index)) = unify types (index, t)
    ensureCompatibleAssignment types (OpCode (Ldfld t _ _ _), OpCode (Stloc index)) = unify types (index, t)
    ensureCompatibleAssignment types (_, OpCode (Stloc index)) = M.insert index Impossible types
    ensureCompatibleAssignment types _ = types


-- Introduce boxing for call, newobj, ret and stloc (when the target local hasn't been inferred)
-- and for stloc with inferred type ensure unbox and remove box as needed.
optimize :: LocalTypes -> [Instruction] -> [Instruction]
optimize types = reverse . introduceBoxing . reverse
  where
    introduceBoxing (op@(OpCode (Tailcall (Call _ _ _ _ _ signature))) : xs) =
      op : ensureStack signature xs
    introduceBoxing (op@(OpCode (Call _ _ _ _ _ signature)) : xs) =
      op : ensureStack signature xs
    introduceBoxing (op@(OpCode (Newobj _ _ _ signature)) : xs) =
      op : ensureStack signature xs
    introduceBoxing (op@(OpCode Ret) : xs) =
      op : ensureStack [Object] xs
    introduceBoxing (OpCode (Castclass Object) : xs) =
      ensureStack [Object] xs
    introduceBoxing (op@(OpCode (Stloc i)) : opb@(OpCode Box{}) : xs) =
      if M.member i types
         then op : introduceBoxing xs
         else op : opb : introduceBoxing xs
    introduceBoxing (opu@(OpCode Unbox_any{}) : op@(OpCode (Ldloc i)) : xs) =
      if M.member i types
         then op : introduceBoxing xs
         else opu : op : introduceBoxing xs
    introduceBoxing (op@(OpCode (Stloc i)) : xs) = op : ensureStack [typeOf i] xs
    introduceBoxing (x : xs) = x : introduceBoxing xs
    introduceBoxing [] = []

    ensureStack :: [PrimitiveType] -> [Instruction] -> [Instruction]
    ensureStack ts xs = loop (reverse ts) (introduceBoxing xs)
      where
        loop (t : ts) (op1@(OpCode Castclass{}) : op2 : xs) =
          op1 : op2 : loop ts xs
        loop (t : ts) (x : xs) =
          case maybeCastTo t x of
            Just c  -> c : x : loop ts xs
            Nothing -> x : loop ts xs
        loop [] xs = xs

    maybeCastTo expectedType instruction = do
      actualType <- typeOnStackAfter instruction
      if expectedType /= actualType
        then Just (if isValueType actualType then box actualType else unbox_any expectedType)
        else Nothing

    typeOnStackAfter :: Instruction -> Maybe PrimitiveType
    typeOnStackAfter (OpCode (Ldloc i)) = Just (typeOf i)
    typeOnStackAfter (OpCode Ldarg{}) = Just Object
    typeOnStackAfter (OpCode (Ldfld fieldType _ _ _)) = Just fieldType
    typeOnStackAfter _ = Nothing

    typeOf localIndex = M.findWithDefault Object localIndex types


removeRedundantSequences :: [Instruction] -> [Instruction]
removeRedundantSequences (OpCode (Ldloc i) : OpCode (Stloc k) : xs)
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
