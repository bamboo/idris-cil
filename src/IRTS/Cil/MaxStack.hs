module IRTS.Cil.MaxStack where

import Language.Cil.Syntax

{- Doesn't honor forward jumps, thus the computed value may exceed the actual stack requirement -}
maxStackFor :: [MethodDecl] -> PrimitiveType -> Int
maxStackFor decls returnType = maximumRunningSum (netStackChange retStackChange <$> ocList decls)
  where
    retStackChange = if returnType /= Void then -1 else 0
    ocList decls = do
      decl <- decls
      case decl of
        OpCode o -> [o]
        _        -> [ ]

maximumRunningSum :: [Int] -> Int
maximumRunningSum = fst . foldl accumulate (0, 0)
  where
    accumulate (rm, rs) e =
      let rs' = rs + e
      in (max rm rs', rs')

netStackChange :: Int -> OpCode -> Int
netStackChange _  Add          = -1
netStackChange _  Add_ovf      = -1
netStackChange _  Add_ovf_un   = -1
netStackChange _  And          = -1
-- arglist?
netStackChange _ (Beq _)       = -2
-- beq.s?
netStackChange _ (Bge _)       = -2
-- bge.s?
-- bge.un?
-- bge.un.s?
netStackChange _ (Bgt _)       = -2
-- bgt.s?
-- bgt.un?
-- bgt.un.s?
netStackChange _ (Ble _)       = -2
-- ble.s?
-- ble.un?
-- ble.un.s?
netStackChange _ (Blt _)       = -2
-- blt.s?
-- bne?
-- bne.s?
-- bne.un?
-- bne.un.s?
netStackChange _ (Box _)       =  0
netStackChange _ (Br  _)       =  0
netStackChange _  Break        =  0
netStackChange _ (Brfalse _)   = -1
netStackChange _ (Brtrue  _)   = -1
netStackChange _ (CallMethod (GenericMethodInstance cc _ _ _ p r)) = netCallStackChange cc p r
netStackChange _ (Call cc r _ _ _ p) = netCallStackChange cc p r
-- NOTE: where did calli go?
netStackChange _ (CallVirt r _ _ _ p)  =
    let retd = if r /= Void then 1 else 0
    in -(length p) + retd - 1 -- always instance call
netStackChange _ (Castclass t) =  0
netStackChange _  Ceq          = -1
netStackChange _  Cgt          = -1
-- cgt.un?
netStackChange _  Ckfinite     =  0
netStackChange _  Clt          = -1
-- clt.un?
-- constrained?
{-netStackChange _  Conv_i       =  0 -}
netStackChange _  Conv_i1      =  0
netStackChange _  Conv_i2      =  0
netStackChange _  Conv_i4      =  0
netStackChange _  Conv_i8      =  0
netStackChange _  Conv_u1      =  0
netStackChange _  Conv_u2      =  0
netStackChange _  Conv_u4      =  0
netStackChange _  Conv_u8      =  0
netStackChange _  Conv_r4      =  0
netStackChange _  Conv_r8      =  0
netStackChange _  Dup          =  1
netStackChange _  Div          = -1
netStackChange _  Div_un       = -1
netStackChange _ (Isinst _)    =  0
netStackChange _ (Ldarg  _)    =  1
netStackChange _  Ldarg_0      =  1
netStackChange _  Ldarg_1      =  1
netStackChange _  Ldarg_2      =  1
netStackChange _  Ldarg_3      =  1
netStackChange _ (LdargN  _)   =  1
netStackChange _ (Ldarga  _)   =  1
netStackChange _ (LdargaN _)   =  1
netStackChange _ (Ldc_i4  _)   =  1
netStackChange _  Ldc_i4_0     =  1
netStackChange _  Ldc_i4_1     =  1
netStackChange _  Ldc_i4_2     =  1
netStackChange _  Ldc_i4_3     =  1
netStackChange _  Ldc_i4_4     =  1
netStackChange _  Ldc_i4_5     =  1
netStackChange _  Ldc_i4_6     =  1
netStackChange _  Ldc_i4_7     =  1
netStackChange _  Ldc_i4_8     =  1
netStackChange _  Ldc_i4_m1    =  1
netStackChange _ (Ldc_i4_s _)  =  1
netStackChange _ (Ldc_i8   _)  =  1
netStackChange _ (Ldc_r4   _)  =  1
netStackChange _ (Ldc_r8   _)  =  1
netStackChange _  Ldelem_i     = -1
netStackChange _  Ldelem_i1    = -1
netStackChange _  Ldelem_i2    = -1
netStackChange _  Ldelem_i4    = -1
netStackChange _  Ldelem_i8    = -1
netStackChange _  Ldelem_u1    = -1
netStackChange _  Ldelem_u2    = -1
netStackChange _  Ldelem_u4    = -1
netStackChange _  Ldelem_u8    = -1
netStackChange _  Ldelem_r4    = -1
netStackChange _  Ldelem_r8    = -1
netStackChange _  Ldelem_ref   = -1
netStackChange _ (Ldelema _ )  = -1
netStackChange _  Ldfld  { }   =  0
netStackChange _  Ldflda { }   =  0
netStackChange _  Ldftn  { }   =  1
-- ldvirtftn?
netStackChange _  Ldind_i      =  0
netStackChange _  Ldind_i1     =  0
netStackChange _  Ldind_i2     =  0
netStackChange _  Ldind_i4     =  0
netStackChange _  Ldind_i8     =  0
netStackChange _  Ldind_r4     =  0
netStackChange _  Ldind_r8     =  0
netStackChange _  Ldind_ref    =  0
netStackChange _  Ldind_u1     =  0
netStackChange _  Ldind_u2     =  0
netStackChange _  Ldind_u4     =  0
-- ldind.u8?
netStackChange _  Ldlen        =  0
netStackChange _ (Ldloc _)     =  1
netStackChange _  Ldloc_0      =  1
netStackChange _  Ldloc_1      =  1
netStackChange _  Ldloc_2      =  1
netStackChange _  Ldloc_3      =  1
netStackChange _ (LdlocN  _)   =  1
netStackChange _ (Ldloca  _)   =  1
netStackChange _ (LdlocaN _)   =  1
netStackChange _  Ldnull       =  1
netStackChange _  Ldsfld { }   =  1 -- docs in language-cil are wrong: it doesn't pop anything, the
                                    -- so-called type ref is part of the instruction operand
netStackChange _  Ldsflda { }  =  1
netStackChange _ (Ldstr    _ ) =  1
netStackChange _ (Ldtoken  _ ) =  1
netStackChange _ (Ldobj    _ ) =  0
-- NOTE: mkrefany?
netStackChange _  Mul          = -1
netStackChange _  Mul_ovf      = -1
netStackChange _  Mul_ovf_un   = -1
netStackChange _  Neg          =  0
netStackChange _ (Newobj _ _ _ p) =
    1 - length p -- actual ctor method retval types are always void,
                 -- but they do push something (obviously)
netStackChange _ (Newarr _)    =  1
netStackChange _  Nop          =  0
netStackChange _  Not          =  0
netStackChange _  Or           = -1
netStackChange _  Xor          = -1
netStackChange _  Pop          = -1
-- NOTE: refanytype?
netStackChange _  Rem          = -1
netStackChange _  Rem_un       = -1
netStackChange retStackChg Ret = retStackChg
-- rethrow?
netStackChange _  Shl          = -1
netStackChange _  Shr          = -1
netStackChange _  Shr_un       = -1
-- sizeof?
-- starg.*?
netStackChange _  Stelem_i     = -3
netStackChange _  Stelem_i1    = -3
netStackChange _  Stelem_i2    = -3
netStackChange _  Stelem_i4    = -3
netStackChange _  Stelem_i8    = -3
netStackChange _  Stelem_r4    = -3
netStackChange _  Stelem_r8    = -3
netStackChange _  Stelem_ref   = -3
netStackChange _  Stfld { }    = -2
netStackChange _  Stind_i      = -2
netStackChange _  Stind_i2     = -2
netStackChange _  Stind_i4     = -2
netStackChange _  Stind_i8     = -2
netStackChange _  Stind_r4     = -2
netStackChange _  Stind_r8     = -2
netStackChange _  Stind_ref    = -2
netStackChange _ (Stloc _)     = -1
netStackChange _  Stloc_0      = -1
netStackChange _  Stloc_1      = -1
netStackChange _  Stloc_2      = -1
netStackChange _  Stloc_3      = -1
netStackChange _ (StlocN _)    = -1
netStackChange _ (Stobj  _)    = -2
netStackChange _  Stsfld { }   = -1
netStackChange _  Sub          = -1
netStackChange _  Sub_ovf      = -1
netStackChange _  Sub_ovf_un   = -1
netStackChange _ (Switch _)    = -1
-- throw?
netStackChange _  Tail         =  0
netStackChange _ (Tailcall _)  =  0
netStackChange _  Throw        =  0
netStackChange _ (Unaligned _) =  0
netStackChange _ (Unbox     _) =  0
netStackChange _ (Unbox_any _) =  0
netStackChange _  Volatile     =  0
-- xor?
netStackChange _ (VolatilePtr _) = 0
netStackChange _ (UnalignedPtr _ _) = 0
netStackChange _ x = error ("unknown: " ++ show x)

netCallStackChange :: [CallConv] -> [PrimitiveType] -> PrimitiveType -> Int
netCallStackChange cc p r =
  let retd = if r /= Void then 1 else 0 in
  let insd = if null cc then 0 else -1 -- this (instance) call (only CC available)
  in -(length p) + retd + insd
