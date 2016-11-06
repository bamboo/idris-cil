{-
(1, 1)
System.Collections.Generic.HashSet`1[System.Tuple`2[System.Int32,System.Int32]]
True
False
True
False
-}

import CIL.FFI

systemCoreTy : String -> CILTy
systemCoreTy = CILTyRef "System.Core"

TupleTy : CILTy
TupleTy = corlibTy "System.Tuple"

IntTupleTy : CILTy
IntTupleTy = CILTyGen TupleTy [CILTyInt32, CILTyInt32]

IntTuple : Type
IntTuple = CIL IntTupleTy

IsA Object IntTuple where {}

CreateIntTuple : Int -> Int -> CIL_IO IntTuple
CreateIntTuple =
  invoke (CILCall (CILGenMethod CCCStatic TupleTy "Create" [CILTyInt32, CILTyInt32]
                                [CILTyGenMethodParam "0", CILTyGenMethodParam "1"]
                                (CILTyGen TupleTy [CILTyGenMethodParam "0", CILTyGenMethodParam "1"])))
         (Int -> Int -> CIL_IO IntTuple)


HashSetTy : CILTy
HashSetTy = systemCoreTy "System.Collections.Generic.HashSet"

IntTupleHashSet : Type
IntTupleHashSet = CIL (CILTyGen HashSetTy [IntTupleTy])

IsA Object IntTupleHashSet where {}

Add : IntTupleHashSet -> IntTuple -> CIL_IO Bool
Add =
  invoke (CILInstanceCustom "Add" [CILTyGenParam "0"] CILTyBool)
         (IntTupleHashSet -> IntTuple -> CIL_IO Bool)


Contains : IntTupleHashSet -> IntTuple -> CIL_IO Bool
Contains =
  invoke (CILInstanceCustom "Contains" [CILTyGenParam "0"] CILTyBool)
         (IntTupleHashSet -> IntTuple -> CIL_IO Bool)

AssemblyReferences : CIL_IO ()
AssemblyReferences =
  assemblyRef "System.Core" "4.0.0.0" "B7 7A 5C 56 19 34 E0 89"

main : CIL_IO ()
main = do
  AssemblyReferences
  CreateIntTuple 1 1 >>= ToString >>= putStrLn
  set <- new (CIL_IO IntTupleHashSet)
  ToString set >>= putStrLn
  CreateIntTuple 1 1 >>= Add set >>= printLn
  CreateIntTuple 1 1 >>= Add set >>= printLn
  CreateIntTuple 1 1 >>= Contains set >>= printLn
  CreateIntTuple 1 2 >>= Contains set >>= printLn

-- Local Variables:
-- idris-load-packages: ("cil")
-- End:
