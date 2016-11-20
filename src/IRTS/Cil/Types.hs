module IRTS.Cil.Types where

import Language.Cil

-- |A CIL instruction.
type Instruction = MethodDecl

isValueType :: PrimitiveType -> Bool
isValueType (ValueType _ _) = True
isValueType Double64 = True
isValueType Float32 = True
isValueType Int32   = True
isValueType Bool    = True
isValueType Char    = True
isValueType _       = False
