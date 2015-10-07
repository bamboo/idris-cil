{-# LANGUAGE RecordWildCards, OverloadedStrings, OverloadedLists, ViewPatterns #-}
module IRTS.CodegenCil (codegenCil) where

import           Control.Monad.RWS.Strict hiding (local)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Char (ord)
import           Data.DList (DList, fromList, toList, append)
import           Data.Function (on)
import           Data.List (partition, sortBy)
import qualified Data.Text as T
import           GHC.Float
import           System.FilePath (takeBaseName, takeExtension, replaceExtension)
import           System.Process (readProcess)

import           IRTS.CodegenCommon
import           IRTS.Lang
import           IRTS.Simplified
import           Idris.Core.CaseTree (CaseType(Shared))
import           Idris.Core.TT

import           Language.Cil
import qualified Language.Cil as Cil

import           IRTS.Cil.FFI
--import           IRTS.Cil.UnreachableCodeRemoval

codegenCil :: CodeGenerator
codegenCil ci = do writeFileUTF8 cilFile cilText
                   when (outputExtension /= ".il") $
                     ilasm cilFile output
  where cilFile = replaceExtension output "il"
        cilText = pr (assemblyFor ci) ""
        output  = outputFile ci
        outputExtension = takeExtension output
        writeFileUTF8 f s = BS.writeFile f $ UTF8.fromString s

ilasm :: String -> String -> IO ()
ilasm input output = readProcess "ilasm" [input, "/output:" ++ output] "" >>= putStr

assemblyFor :: CodegenInfo -> Assembly
assemblyFor ci = Assembly [mscorlibRef] asmName types
  where asmName = quoted $ takeBaseName (outputFile ci)
        types   = mainModule : runtime ++ exports
        mainModule = moduleFor ci
        runtime    = [sconType, nothingType]
        exports    = exportedTypes ci

moduleFor :: CodegenInfo -> TypeDef
moduleFor ci = classDef [CaPrivate] moduleName noExtends noImplements [] methods []
  where methods       = map method declsWithBody -- removeUnreachable $ map method declsWithBody
        declsWithBody = filter hasBody decls
        decls         = map snd $ simpleDecls ci
        hasBody (SFun _ _ _ SNothing) = False
        hasBody _                     = True

moduleName :: String
moduleName = "'λΠ'"

method :: SDecl -> MethodDef
method decl@(SFun name ps _ sexp) = Method attrs retType (cilName name) parameters (toList body)
  where attrs      = [MaStatic, MaAssembly]
        retType    = if isEntryPoint then Cil.Void else Cil.Object
        parameters = map param ps
        param n    = Param Nothing Cil.Object (cilName n)
        body       = let (sexpRetType, CodegenState _ lc, cilForSexp) = cilFor decl sexp
                     in if isEntryPoint
                          then
                            [entryPoint]
                              `append` fromList (removeLastTailCall $ toList cilForSexp)
                              `append` [pop, ret]
                          else
                            [comment (show decl)]
                              `append` cilForSexp
                              `append` (if isValueType sexpRetType then [ box sexpRetType, ret ] else [ ret ])
        isEntryPoint = name == entryPointName
        removeLastTailCall :: [MethodDecl] -> [MethodDecl]
        removeLastTailCall [OpCode (Tailcall e), OpCode Ret, OpCode Ldnull] = [OpCode e]
        removeLastTailCall (x:xs) = x:removeLastTailCall xs
        removeLastTailCall _ = error "Entry point should end in tail call"

data CilExport = CilFun  !MethodDef
               | CilType !TypeDef

exportedTypes :: CodegenInfo -> [TypeDef]
exportedTypes ci = concatMap exports (exportDecls ci)
  where exports :: ExportIFace -> [TypeDef]
        exports (Export (NS (UN (T.unpack -> "FFI_CIL")) _) exportedDataType es) =
            let cilExports = map cilExport es
                (cilFuns, cilTypes) = partition isCilFun cilExports
                methods = map (\(CilFun m) -> m) cilFuns
                types   = map (\(CilType t) -> t) cilTypes
            in publicClass exportedDataType methods : types
          where isCilFun (CilFun _) = True
                isCilFun _          = False
                publicClass name methods = classDef [CaPublic] name noExtends noImplements [] methods []
        exports e = error $ "Unsupported Export: " ++ show e

cilExport :: Export -> CilExport
cilExport (ExportFun fn@(NS n _) desc rt ps) = CilFun $ Method attrs retType exportName parameters body
  where attrs      = [MaPublic, MaStatic]
        retType    = foreignTypeToCilType rt
        exportName = if null alias then cilName n else alias
        alias      = case desc of
                       FApp (UN (T.unpack -> "CILExport")) (FStr a:_) -> a
                       _ -> ""
        parameters = zipWith param [(0 :: Int)..] paramTypes
        param i t  = Param Nothing t ("p" ++ show i)
        paramTypes = map foreignTypeToCilType ps
        body       = if isIO rt
                        then loadNothing : dup : loadArgs ++ [invoke, runIO, popBoxOrCast, ret]
                        else loadArgs ++ [invoke, popBoxOrCast, ret]
        runIO      = call [] Cil.Object "" moduleName "call__IO" [Cil.Object, Cil.Object, Cil.Object]
        loadArgs   = concatMap loadArg (zip [0..] paramTypes)
        -- Exported data types are encoded as structs with a single `ptr` field
        loadArg (i, ValueType "" exportedDataType) = [ ldarga i
                                                     , ldfld Cil.Object "" exportedDataType "ptr" ]
        loadArg (i, t) = ldarg i : [box t | isValueType t]
        invoke     = call [] Cil.Object "" moduleName (cilName fn) (map (const Cil.Object) ps)
        popBoxOrCast = case retType of
                         Void -> pop
                         -- Exported data types are encoded as structs with a single `ptr` field
                         ValueType "" exportedDataType -> newobj "" exportedDataType [Cil.Object]
                         t | isValueType t -> unbox_any t
                         t -> castclass t

cilExport (ExportData (FStr exportedDataType)) = CilType $ publicStruct exportedDataType [ptr] [ctor] []
  where ptr  = Field [FaAssembly, FaInitOnly] Cil.Object "ptr"
        ctor = Constructor [MaAssembly] Void [Param Nothing Cil.Object "ptr"]
                 [ ldarg 0
                 , ldarg 1
                 , stfld Cil.Object "" exportedDataType "ptr"
                 , ret ]

cilExport e = error $ "invalid export: " ++ show e

data CodegenInput = CodegenInput !SDecl !Int -- cached param count

type TypedLocal = (Int, (PrimitiveType, String))
data CodegenState = CodegenState { nextSuffix :: !Int
                                 , locals :: [TypedLocal] }

type CodegenOutput = DList MethodDecl

type CilCodegen a = RWS CodegenInput CodegenOutput CodegenState a

cilFor :: SDecl -> SExp -> (PrimitiveType, CodegenState, CodegenOutput)
cilFor decl@(SFun _ params _ _) sexp = runRWS (cil sexp)
                                               (CodegenInput decl paramCount)
                                               (CodegenState 0 [])
  where paramCount = length params

cil :: SExp -> CilCodegen PrimitiveType
cil (SLet (Loc i) v e) = do
  ty <- case v of
    SNothing -> do tell [ loadNothing ]
                   return Cil.Object
    _        -> cil v
  storeLocal (i, ty)
  ty <- cil e
  modify popLocal
  return ty
  where popLocal (CodegenState nextSuffix (_:locals)) = CodegenState nextSuffix locals
cil (SUpdate _ v) = cil v
cil (SV v)        = load v
cil (SConst c)    = cgConst c
cil (SOp op args) = cgOp op args
cil SNothing      = do
  throwException "SNothing"
  return Cil.Object

-- Special constructors: True, False
cil (SCon _ 0 n []) | n == boolFalse = do tell [ ldc_i4 0 ]
                                          return Bool
cil (SCon _ 1 n []) | n == boolTrue  = do tell [ ldc_i4 1 ]
                                          return Bool

-- General constructors
cil (SCon Nothing t _ fs) = do
  tell [ ldc t
       , ldc $ length fs
       , newarr Cil.Object ]
  mapM_ storeElement (zip [0..] fs)
  tell [ newobj "" "SCon" [Int32, array] ]
  return Cil.Object
  where storeElement (i, f) = do
          tell [ dup
               , ldc_i4 i ]
          ty <- load f
          boxIfValueType ty
          tell [ stelem_ref ]

-- ifThenElse
cil (SCase Shared v [ SConCase _ 0 nFalse [] elseAlt
                    , SConCase _ 1 nTrue  [] thenAlt ]) | nFalse == boolFalse && nTrue == boolTrue =
  cgIfThenElse v Bool thenAlt elseAlt $
    \thenLabel -> tell [ brtrue thenLabel ]

cil (SCase Shared v [ SConCase _ tag _ [] thenAlt, SDefaultCase elseAlt ]) =
  cgIfThenElse v Cil.Object thenAlt elseAlt $
    \thenLabel -> do loadSConTag
                     tell [ ldc tag
                          , beq thenLabel ]

-- In some situations idris gives us a SCase with two default clauses
cil (SCase Shared v [t@SConstCase{}, e@SDefaultCase{}, SDefaultCase{}]) = cil (SCase Shared v [t, e])

cil (SCase Shared v [SConstCase c thenAlt, SDefaultCase elseAlt]) =
  cgIfThenElse v (constType c) thenAlt elseAlt $ \thenLabel ->
    cgBranchEq c thenLabel

cil (SCase Shared v [c@SConCase{}]) = cgSConCase v c

cil e@(SCase Shared v alts) = let (cases, defaultCase) = partition caseType alts
                              in case defaultCase of
                                   [] -> cgCase v (sorted cases ++ [SDefaultCase SNothing])
                                   _  -> cgCase v (sorted cases ++ defaultCase)
   where sorted = sortBy (compare `on` tag)
         tag (SConCase _ t _ _ _) = t
         tag (SConstCase (I t) _) = t
         tag c                    = unsupportedCase c
         caseType SDefaultCase{}  = False
         caseType _               = True
         unsupportedCase c        = error $ show c ++ " in\n" ++ show e

cil (SChkCase _ [SDefaultCase e]) = cil e
cil (SChkCase v alts) = cgCase v alts

cil (SApp isTailCall n args) = do
  forM_ args (load >=> boxIfValueType)
  if isTailCall
    then do tell [ tailcall app, ret, ldnull ]
            return Cil.Object
    else do tell [ app ]
            return Cil.Object
  where app = call [] Cil.Object "" moduleName (cilName n) (map (const Cil.Object) args)

cil (SForeign retDesc desc args) = emit $ parseDescriptor desc
  where emit :: CILForeign -> CilCodegen PrimitiveType
        emit (CILTypeOf t) = do
          tell [ ldtoken t
               , call [] runtimeType "mscorlib" "System.Type" "GetTypeFromHandle" [runtimeTypeHandle] ]
          return Cil.Object
        emit (CILEnumValueOf t i) = do
          tell [ ldc i ]
          return t
        emit ffi     = do
          mapM_ loadArg (zip (map snd args) sig)
          case ffi of
            CILConstructor ->
              let (assemblyName, typeName) = assemblyNameAndTypeFrom retType
              in tell [ newobj   assemblyName typeName sig ]
            CILInstance fn ->
              let (assemblyName, typeName) = assemblyNameAndTypeFrom $ head sig
              in tell [ callvirt retType assemblyName typeName fn (Prelude.tail sig) ]
            CILInstanceField fn ->
              let (assemblyName, typeName) = assemblyNameAndTypeFrom $ head sig
              in tell [ ldfld retType assemblyName typeName fn ]
            CILStatic declType fn ->
              let (assemblyName, typeName) = assemblyNameAndTypeFrom declType
              in tell [ call []  retType assemblyName typeName fn sig ]
            CILStaticField declType fn ->
              let (assemblyName, typeName) = assemblyNameAndTypeFrom declType
              in tell [ ldsfld   retType assemblyName typeName fn ]
            _ -> error $ "unsupported ffi descriptor: " ++ show ffi
          acceptBoxOrPush retType
          return retType
        loadArg :: (LVar, PrimitiveType) -> CilCodegen ()
        loadArg (loc, t) = do load loc
                              castOrUnbox t
        acceptBoxOrPush :: PrimitiveType -> CilCodegen ()
        acceptBoxOrPush Void              = tell [ loadNothing ]
        --acceptBoxOrPush t | isValueType t = tell [ box t ]
        acceptBoxOrPush _                 = return ()
        sig                               = map (foreignTypeToCilType . fst) args
        retType                           = foreignTypeToCilType retDesc

cil e = do
  unsupported "expression" e
  return Cil.Object

castOrUnbox :: PrimitiveType -> CilCodegen ()
castOrUnbox t =
  tell [
    if isValueType t
       then unbox_any t
       else castclass t ]

isValueType :: PrimitiveType -> Bool
isValueType (ValueType _ _) = True
isValueType Float32 = True
isValueType Int32   = True
isValueType Bool    = True
isValueType Char    = True
isValueType _       = False

constType :: Const -> PrimitiveType
constType (BI _) = Int32 -- for now
constType (I _) = Int32
constType (Fl _) = Float32
constType (Ch _) = Char
constType (Str _) = String
constType (B8 _) = Char
constType (B32 _) = Int32
constType c = error $ "Unmapped Const: " ++ constDocs c

loadSConTag :: CilCodegen ()
loadSConTag = tell [ castclass (ReferenceType "" "SCon")
                   , ldfld Int32 "" "SCon" "tag" ]

cgIfThenElse :: LVar -> PrimitiveType -> SExp -> SExp -> (String -> CilCodegen ()) -> CilCodegen PrimitiveType
cgIfThenElse v@(Loc i) ty thenAlt elseAlt cgBranch = do
  thenLabel <- gensym "THEN"
  endLabel  <- gensym "END"
  CodegenState _ locals <- get
  let lty = lookup i locals
  loadAs ty v
  cgBranch thenLabel
  elseTy <- cil elseAlt
  boxIfValueType elseTy
  tell [ br endLabel
       , label thenLabel ]
  thenTy <- cil thenAlt
  boxIfValueType thenTy
  tell [ label endLabel ]
  return Cil.Object -- unable to determine ifThenElse type, hence boxing and Cil.Object...

cgConst :: Const -> CilCodegen PrimitiveType
cgConst (Str s) = do
  tell [ ldstr s ]
  return String

cgConst (I i)   = cgConst . BI . fromIntegral $ i

cgConst (BI i)  = do
  tell [ ldc i ]
  return Int32

cgConst (Ch c)  = do
  tell [ ldc $ ord c ]
  return Char

cgConst (Fl d)  = do
  tell [ ldc_r4 (double2Float d) ]
  return Float32

cgConst c = do
  unsupported "const" c
  return Cil.Object
{-
  = I Int
  | BI Integer
  | Fl Double
  | Ch Char
  | Str String
  | B8 GHC.Word.Word8
  | B16 GHC.Word.Word16
  | B32 GHC.Word.Word32
  | B64 GHC.Word.Word64
  | AType ArithTy
  | StrType
  | WorldType
  | TheWorld
  | VoidType
  | Forgot
-}

cgCase :: LVar -> [SAlt] -> CilCodegen PrimitiveType
cgCase v alts@(SConstCase{} : _) = cgSwitchCase v alts loadTag altTag
  where loadTag = tell [ unbox_any Int32 ]
        altTag (SConstCase (I t) _) = t
        altTag alt = error $ "expecting (SConstCase (I t)) got: " ++ show alt

cgCase v alts = cgSwitchCase v alts loadTag altTag
  where loadTag = loadSConTag
        altTag (SConCase _ t _ _ _) = t
        altTag alt = error $ "expecting SConCase got: " ++ show alt

cgSwitchCase :: LVar -> [SAlt] -> CilCodegen () -> (SAlt -> Int) -> CilCodegen PrimitiveType
cgSwitchCase val alts loadTag altTag  | canBuildJumpTable alts = do
  labelPrefix <- gensym "L"
  let labels = map ((labelPrefix++) . show) [0..(length alts - 1)]
  endLabel <- gensym "END"
  load val
  loadTag
  tell [ ldc baseTag
       , sub
       , switch labels ]
  mapM_ (cgAlt endLabel val) (zip labels alts)
  tell [ label endLabel ]
  return Cil.Object
  where canBuildJumpTable (a:as) = canBuildJumpTable' (altTag a) as
        canBuildJumpTable _      = False
        canBuildJumpTable' _ [SDefaultCase _]     = True
        canBuildJumpTable' t (a:as) | t' == t + 1 = canBuildJumpTable' t' as where t' = altTag a
        canBuildJumpTable' _ _                    = False
        baseTag = altTag (head alts)

cgSwitchCase _ _ _ alts = do
  unsupported "switch case alternatives" alts
  return Cil.Object

cgAlt :: Label -> LVar -> (Label, SAlt) -> CilCodegen PrimitiveType
cgAlt end v (l, alt) = do
  tell [ label l ]
  ty <- cg alt
  boxIfValueType ty
  tell [ br end ]
  return Cil.Object -- ehh
  where cg (SConstCase _ e) = cil e
        cg (SDefaultCase e) = cil e
        cg c                = cgSConCase v c

storeLocal :: (Int, PrimitiveType) -> CilCodegen ()
storeLocal (i, ty) = do
  CodegenState _ locals <- get
  sym <- gensym $ "loc" ++ show i ++ "_"
  modify (addLocal sym)
  localCount <- gets (\(CodegenState _ locals) -> length locals)
  tell [ localsInit [ Local ty sym ], stlocN sym ]
  where addLocal sym CodegenState{..} = CodegenState nextSuffix ((i, (ty, sym)):locals)

cgBranchEq :: Const -> String -> CilCodegen ()
cgBranchEq (BI i) target = cgBranchEq (I . fromIntegral $ i) target
cgBranchEq (Ch c) target =
  tell [ ldc $ ord c
       , beq target ]
cgBranchEq (I i) target =
  tell [ ldc i
       , beq target ]
cgBranchEq c _ = unsupported "branch on const" c

cgSConCase :: LVar -> SAlt -> CilCodegen PrimitiveType
cgSConCase v (SConCase offset _ _ fs sexp) = do
  unless (null fs) $ do
    load v
    tell [ castclass sconTypeRef
         , ldfld array "" "SCon" "fields" ]
    mapM_ project (zip [0..length fs - 1] [offset..])
    tell [ pop ]
  cil sexp
  where project (f, l) = do tell [ dup
                                 , ldc f
                                 , ldelem_ref ]
                            storeLocal (l, Cil.Object)
cgSConCase _ c = do
  unsupported "SConCase" c
  return Cil.Object

cgOp :: PrimFn -> [LVar] -> CilCodegen PrimitiveType
cgOp LWriteStr [_, s] = do
  load s
  tell [ castclass String
       , call [] Void "mscorlib" "System.Console" "Write" [String]
       , loadNothing ]
  return Cil.Object

cgOp LReadStr [_] = do
  tell [ call [] String "mscorlib" "System.Console" "ReadLine" [] ]
  return String

cgOp LStrRev [s] = do
  loadString s
  tell [ callvirt charArray "mscorlib" "System.String" "ToCharArray" []
       , dup
       , call [] Void "mscorlib" "System.Array" "Reverse" [ReferenceType "mscorlib" "System.Array"]
       , newobj "mscorlib" "System.String" [charArray] ]
  return String

cgOp LStrLen [s] = do
  loadString s
  tell [ callvirt Int32 "mscorlib" "System.String" "get_Length" [] ]
  return Int32

cgOp LStrConcat args = do
  forM_ args loadString
  tell [ call [] String "mscorlib" "System.String" "Concat" (map (const String) args) ]
  return String

cgOp LStrCons [h, t] = do
  loadAs Char h
  tell [ call [] String "mscorlib" "System.Char" "ToString" [Char] ]
  loadString t
  tell [ call [] String "mscorlib" "System.String" "Concat" [String, String] ]
  return String

cgOp LStrSubstr [index, count, s] = do
  loadString s
  loadAs Int32 index
  loadAs Int32 count
  tell [ callvirt String "mscorlib" "System.String" "Substring" [Int32, Int32] ]
  return String

cgOp LStrEq args = do
  forM_ args loadString
  tell [ call [] Bool "mscorlib" "System.String" "op_Equality" (map (const String) args) ]
  return Bool

cgOp LStrHead [v] = do
  loadString v
  tell [ ldc_i4 0
       , call [CcInstance] Char "mscorlib" "System.String" "get_Chars" [Int32]]
  return Char

cgOp LStrTail [v] = do
  loadString v
  tell [ ldc_i4 1
       , call [CcInstance] String "mscorlib" "System.String" "Substring" [Int32] ]
  return String

cgOp (LStrInt ITNative) [v] = do
  val <- gensym "val"
  tell [ localsInit [ Local Int32 val ]
       , ldc_i4 0
       , stlocN val ]
  loadString v
  tell [ ldlocaN val
       , call [] Bool "mscorlib" "System.Int32" "TryParse" [String, ByRef Int32]
       , pop
       , ldlocN val ]
  return Int32

cgOp (LStrInt i) [_] = do
  unsupported "LStrInt" i
  return Int32

-- cgOp (LChInt ITNative) [c] = do
--   load c
--   tell [ unbox_any systemChar
--        , boxInt32 ]

-- cgOp (LEq (ATInt ITChar)) args = do
--   forM_ args loadChar
--   tell [ ceq
--        , boxBoolean ]

-- cgOp (LSLt (ATInt ITChar)) args = do
--   forM_ args loadChar
--   tell [ clt
--        , boxBoolean ]

cgOp (LSExt ITNative ITBig) [i]  = load i
cgOp (LZExt ITNative ITBig) [i]  = load i
cgOp (LPlus (ATInt _))      args = intOp add args
cgOp (LMinus (ATInt _))     args = intOp sub args
cgOp (LTimes (ATInt _))     args = intOp mul args
cgOp (LEq (ATInt ITChar))   args = primitiveOp Char Int32 ceq args
cgOp (LEq (ATInt _))        args = intOp ceq args
cgOp (LSLt (ATInt _))       args = intOp clt args
cgOp (LIntStr _)            [i]  = primitiveToString i
cgOp (LIntFloat _)          [i]  = convert Int32 Float32 "ToSingle" i
cgOp (LTimes ATFloat)       args = floatOp mul args
cgOp (LSDiv ATFloat)        args = floatOp Cil.div args
cgOp (LPlus ATFloat)        args = floatOp add args
cgOp (LMinus ATFloat)       args = floatOp sub args
cgOp LFloatStr              [f]  = primitiveToString f
cgOp (LExternal nul)        [] | nul == sUN "prim__null" = do tell [ ldnull ]
                                                              return Cil.Object
cgOp o _ = do
  unsupported "operation" o
  return Cil.Object

primitiveToString :: LVar -> CilCodegen PrimitiveType
primitiveToString p = do
  ty <- load p
  tell [ box ty, objectToString ]
  return String

objectToString :: MethodDecl
objectToString = callvirt String "mscorlib" "System.Object" "ToString" []

unsupported :: Show a => String -> a -> CilCodegen ()
unsupported desc v = do
  (CodegenInput decl _) <- ask
  throwException $ "Unsupported " ++ desc ++ " `" ++ show v ++ "' in\n" ++ show decl

throwException :: String -> CilCodegen ()
throwException message =
  tell [ ldstr message
       , newobj "mscorlib" "System.Exception" [String]
       , throw
       , ldnull ]

gensym :: String -> CilCodegen String
gensym prefix = do
  (CodegenState suffix locals) <- get
  put (CodegenState (suffix + 1) locals)
  return $ prefix ++ show suffix

intOp :: MethodDecl -> [LVar] -> CilCodegen PrimitiveType
intOp = numOp Int32

floatOp :: MethodDecl -> [LVar] -> CilCodegen PrimitiveType
floatOp = numOp Float32

numOp :: PrimitiveType -> MethodDecl -> [LVar] -> CilCodegen PrimitiveType
numOp t = primitiveOp t t

primitiveOp :: PrimitiveType -> PrimitiveType -> MethodDecl -> [LVar] -> CilCodegen PrimitiveType
primitiveOp argT resT op args = do
  forM_ args (loadAs argT)
  tell [ op ]
  return resT

convert :: PrimitiveType -> PrimitiveType -> String -> LVar -> CilCodegen PrimitiveType
convert from to fn arg = do
  loadAs from arg
  tell [ call [] to "mscorlib" "System.Convert" fn [from] ]
  return to

boxInt32, boxFloat32, boxChar, boxBoolean :: MethodDecl
boxInt32   = box Int32
boxFloat32 = box Float32
boxChar    = box Char
boxBoolean = box Bool

boxIfValueType :: PrimitiveType -> CilCodegen ()
boxIfValueType ty = when (isValueType ty) $ tell [ box ty ]

loadAs :: PrimitiveType -> LVar -> CilCodegen ()
loadAs ty l = do
  loadedTy <- load l
  unless (isValueType loadedTy) $ tell [ unbox_any ty ]

loadString :: LVar -> CilCodegen ()
loadString l = do
  load l
  tell [ castclass String ]

ldc :: (Integral n) => n -> MethodDecl
ldc = ldc_i4 . fromIntegral

load :: LVar -> CilCodegen PrimitiveType
load (Loc i) = do
  CodegenState _ locals <- get
  case lookup i locals of
    Just (ty, sym) -> do
      tell [ ldlocN sym ]
      return ty
    Nothing -> do
      tell [ ldarg i ]
      return Cil.Object
load v = do
  unsupported "LVar" v
  return Cil.Object

entryPointName :: Name
entryPointName = MN 0 "runMain"
--entryPointName = NS (UN "main") ["Main"]

cilName :: Name -> String
cilName = quoted . T.unpack . showName
  where showName (NS n ns) = T.intercalate "." . reverse $ showName n : ns
        showName (UN t)    = t
        showName (MN i t)  = T.concat [t, T.pack $ show i]
        showName (SN sn)   = T.pack $ show sn
        showName e = error $ "Unsupported name `" ++ show e ++ "'"

loadNothing :: MethodDecl
loadNothing = ldsfld Cil.Object "" "Nothing" "Default"

nothingType :: TypeDef
nothingType = classDef [CaPrivate] className noExtends noImplements
                    [nothing] [defaultCtorDef, cctor] []
  where className = "Nothing"
        nothing   = Field [FaStatic, FaPublic, FaInitOnly] Cil.Object "Default"
        cctor     = Constructor [MaStatic] Void []
                      [ newobj "" className []
                      , stsfld Cil.Object "" className "Default"
                      , ret ]

defaultCtorDef :: MethodDef
defaultCtorDef = Constructor [MaPublic] Void []
                   [ ldarg 0
                   , call [CcInstance] Void "" "object" ".ctor" []
                   , ret ]

sconType :: TypeDef
sconType = classDef [CaPrivate] className noExtends noImplements
                    [sconTag, sconFields] [sconCtor, sconToString] []
  where className  = "SCon"
        sconTag    = Field [FaPublic, FaInitOnly] Int32 "tag"
        sconFields = Field [FaPublic, FaInitOnly] array "fields"
        sconCtor   = Constructor [MaPublic] Void [ Param Nothing Int32 "tag"
                                                 , Param Nothing array "fields" ]
                     [ ldarg 0
                     , call [CcInstance] Void "" "object" ".ctor" []
                     , ldarg 0
                     , ldarg 1
                     , stfld Int32 "" className "tag"
                     , ldarg 0
                     , ldarg 2
                     , stfld array "" className "fields"
                     , ret ]
        sconToString = Method [MaPublic, MaVirtual] String "ToString" []
                       [ ldstr "SCon "
                       , ldarg 0
                       , ldfld Int32 "" className "tag"
                       , boxInt32
                       , objectToString
                       , call [] String "mscorlib" "System.String" "Concat" [String, String]
                       , ret ]

sconTypeRef :: PrimitiveType
sconTypeRef = ReferenceType "" "SCon"

publicStruct :: TypeName -> [FieldDef] -> [MethodDef] -> [TypeDef] -> TypeDef
publicStruct name = classDef [CaPublic] name (extends "[mscorlib]System.ValueType") noImplements

array, charArray :: PrimitiveType
array = Array Cil.Object
charArray = Array Char

runtimeType :: PrimitiveType
runtimeType = ReferenceType "mscorlib" "System.Type"

runtimeTypeHandle :: PrimitiveType
runtimeTypeHandle = ValueType "mscorlib" "System.RuntimeTypeHandle"

boolFalse, boolTrue :: Name
boolFalse = NS (UN "False") ["Bool", "Prelude"]
boolTrue  = NS (UN "True")  ["Bool", "Prelude"]

quoted :: String -> String
quoted n = "'" ++ concatMap validChar n ++ "'"
  where validChar :: Char -> String
        validChar c = if c == '\''
                         then "\\'"
                         else [c]
