{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}
module IRTS.CodegenCil (codegenCil) where

import           Control.Monad.RWS.Strict hiding (local)
import           Control.Monad.State.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Char (ord)
import           Data.DList (DList, fromList, toList, append)
import           Data.Function (on)
import           Data.List (partition, sortBy)
import qualified Data.Map.Strict as M
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

type DelegateOutput = M.Map ForeignFunctionType MethodDef

type DelegateWriter = State DelegateOutput

assemblyFor :: CodegenInfo -> Assembly
assemblyFor ci = Assembly [mscorlibRef] asmName types
  where asmName = quoted $ takeBaseName (outputFile ci)
        types   = typesFor ci

typesFor :: CodegenInfo -> [TypeDef]
typesFor ci =
  let (mainModule, delegates) = runState (moduleFor ci) M.empty
  in mainModule : sconType (M.elems delegates) : nothingType : exportedTypes ci

moduleFor :: CodegenInfo -> DelegateWriter TypeDef
moduleFor ci = do methods <- mapM method declsWithBody
                  return $ classDef [CaPrivate] moduleName noExtends noImplements [] methods []
  where declsWithBody = filter hasBody decls
        decls         = map snd $ simpleDecls ci
        hasBody (SFun _ _ _ SNothing) = False
        hasBody _                     = True

moduleName :: String
moduleName = "'λΠ'"

method :: SDecl -> DelegateWriter MethodDef
method decl@(SFun name ps _ sexp) = do
  delegates <- get
  let (CodegenState _ lc delegates', cilForSexp) = cilFor delegates decl sexp
      body = if isEntryPoint
             then
               [entryPoint]
                 `append` locals lc
                 `append` fromList (removeLastTailCall $ toList cilForSexp)
                 `append` [pop, ret]
             else
               [comment (show decl)]
                 `append` locals lc
                 `append` cilForSexp
                 `append` [ret]
  put delegates'
  return $ Method attrs retType (cilName name) parameters (toList body)
  where attrs      = [MaStatic, MaAssembly]
        retType    = if isEntryPoint then Cil.Void else Cil.Object
        parameters = map param ps
        param n    = Param Nothing Cil.Object (cilName n)
        locals lc  = fromList [localsInit $ map local [0..(lc - 1)] | lc > 0]
        local i    = Local Cil.Object ("l" ++ show i)
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
cilExport (ExportFun fn@(NS n _) desc rt ps) = CilFun f
  where f          = delegateFunction [MaPublic, MaStatic] retType exportName paramTypes io invocation
        exportName = if null alias then cilName n else alias
        alias      = case desc of
                       FApp (UN (T.unpack -> "CILExport")) (FStr a:_) -> a
                       _ -> ""
        invocation = loadArgs ++ [ call [] Cil.Object "" moduleName (cilName fn) (map (const Cil.Object) ps) ]
        loadArgs   = concatMap loadArg (zip [0..] paramTypes)
        paramTypes = map foreignType ps
        retType    = foreignType rt
        io         = isIO rt

cilExport (ExportData (FStr exportedDataType)) = CilType $ publicStruct exportedDataType [ptr] [ctor] []
  where ptr  = Field [FaAssembly, FaInitOnly] Cil.Object "ptr"
        ctor = Constructor [MaAssembly] Void [Param Nothing Cil.Object "ptr"]
                 [ ldarg 0
                 , ldarg 1
                 , stfld Cil.Object "" exportedDataType "ptr"
                 , ret ]

cilExport e = error $ "invalid export: " ++ show e


data CodegenInput = CodegenInput !SDecl !Int -- cached param count

data CodegenState = CodegenState { nextSuffix :: !Int
                                 , localCount :: !Int
                                 , delegates  :: !DelegateOutput }

type CodegenOutput = DList MethodDecl

type CilCodegen a = RWS CodegenInput CodegenOutput CodegenState a

cilFor :: DelegateOutput -> SDecl -> SExp -> (CodegenState, CodegenOutput)
cilFor delegates decl@(SFun _ params _ _) sexp = execRWS (cil sexp)
                                                         (CodegenInput decl paramCount)
                                                         (CodegenState 0 0 delegates)
  where paramCount = length params

cil :: SExp -> CilCodegen ()
cil (SLet (Loc i) v e) = do
  case v of
    SNothing -> tell [ loadNothing ]
    _        -> cil v
  localIndex i >>= storeLocal
  cil e

cil (SUpdate _ v) = cil v
cil (SV v)        = load v
cil (SConst c)    = cgConst c
cil (SOp op args) = cgOp op args
cil SNothing      = throwException "SNothing"

-- Special constructors: True, False
cil (SCon _ 0 n []) | n == boolFalse = tell [ ldc_i4 0, boxBoolean ]
cil (SCon _ 1 n []) | n == boolTrue  = tell [ ldc_i4 1, boxBoolean ]

-- General constructors
cil (SCon Nothing t _ fs) = do
  tell [ ldc t
       , ldc $ length fs
       , newarr Cil.Object ]
  mapM_ storeElement (zip [0..] fs)
  tell [ newobj "" "SCon" [Int32, array] ]
  where storeElement (i, f) = do
          tell [ dup
               , ldc_i4 i ]
          load f
          tell [ stelem_ref ]

-- ifThenElse
cil (SCase Shared v [ SConCase _ 0 nFalse [] elseAlt
                    , SConCase _ 1 nTrue  [] thenAlt ]) | nFalse == boolFalse && nTrue == boolTrue =
  cgIfThenElse v thenAlt elseAlt $
    \thenLabel -> tell [ unbox_any Bool
                       , brtrue thenLabel ]

cil (SCase Shared v [ SConCase _ tag _ [] thenAlt, SDefaultCase elseAlt ]) =
  cgIfThenElse v thenAlt elseAlt $
    \thenLabel -> do loadSConTag
                     tell [ ldc tag
                          , beq thenLabel ]

-- In some situations idris gives us a SCase with two default clauses
cil (SCase Shared v [t@SConstCase{}, e@SDefaultCase{}, SDefaultCase{}]) = cil (SCase Shared v [t, e])

cil (SCase Shared v [SConstCase c thenAlt, SDefaultCase elseAlt]) =
  cgIfThenElse v thenAlt elseAlt $ \thenLabel ->
    cgBranchEq c thenLabel

cil (SCase Shared v [c@SConCase{}]) = cgSConCase v c

cil e@(SCase Shared v alts) = let (cases, defaultCase) = partition caseType alts
                              in case defaultCase of
                                   [] -> cgCase False v (sorted cases ++ [SDefaultCase SNothing])
                                   _  -> cgCase False v (sorted cases ++ defaultCase)
   where sorted = sortBy (compare `on` tag)
         tag (SConCase _ t _ _ _) = t
         tag (SConstCase (I t) _) = t
         tag c                    = unsupportedCase c
         caseType SDefaultCase{}  = False
         caseType _               = True
         unsupportedCase c        = error $ show c ++ " in\n" ++ show e

cil (SChkCase _ [SDefaultCase e]) = cil e
cil (SChkCase v alts) = cgCase True v alts

cil (SApp isTailCall n args) = do
  forM_ args load
  if isTailCall
    then tell [ tailcall app, ret, ldnull ]
    else tell [ app ]
  where app = call [] Cil.Object "" moduleName (cilName n) (map (const Cil.Object) args)

cil (SForeign retDesc desc args) = emit $ parseDescriptor desc
  where emit :: CILForeign -> CilCodegen ()
        emit (CILDelegate t) =
          cilDelegate t retDesc args
        emit (CILTypeOf t) =
          cilTypeOf t
        emit (CILEnumValueOf t i) =
          tell [ ldc i
               , box t ]
        emit ffi     = do
          mapM_ loadLVar (zip (map snd args) sig)
          case ffi of
            CILConstructor ->
              let (assemblyName, typeName) = assemblyNameAndTypeFrom retType
              in tell [ newobj   assemblyName typeName sig ]
            CILInstance fn ->
              let (assemblyName, typeName) = assemblyNameAndTypeFrom $ head sig
              in tell [ callvirt retType assemblyName typeName fn (Prelude.tail sig) ]
            CILInstanceCustom fn sig' retType' ->
              let (assemblyName, typeName) = assemblyNameAndTypeFrom $ head sig
              in tell [ callvirt retType' assemblyName typeName fn sig' ]
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
        loadLVar :: (LVar, PrimitiveType) -> CilCodegen ()
        loadLVar (loc, t) = do load loc
                               castOrUnbox t
        acceptBoxOrPush :: PrimitiveType -> CilCodegen ()
        acceptBoxOrPush Void              = tell [ loadNothing ]
        acceptBoxOrPush t | isValueType t = tell [ box t ]
        acceptBoxOrPush _                 = return ()
        sig                               = map (foreignType . fst) args
        retType                           = foreignType retDesc

cil e = unsupported "expression" e

-- Delegates are emitted as instance functions of the general SCon data type
-- so we can avoid the overhead of an additional closure object at runtime
cilDelegate :: PrimitiveType -> FDesc -> [(FDesc, LVar)] -> CilCodegen ()
cilDelegate delegateTy retDesc [(_, fnArg)] = do
  let fft = parseForeignFunctionType retDesc
  fn <- delegateMethodFor fft
  load fnArg
  let (delegateAsm, delegateTyName) = assemblyNameAndTypeFrom delegateTy
  let ForeignFunctionType{..} = fft
  tell [ castclass sconTypeRef
       , ldftn_instance returnType "" "SCon" fn parameterTypes
       , newobj delegateAsm delegateTyName [Cil.Object, IntPtr] ]
cilDelegate _ retDesc _ = unsupported "delegate" retDesc

delegateMethodFor :: ForeignFunctionType -> CilCodegen String
delegateMethodFor fft = do
  st@(CodegenState _ _ delegates) <- get
  case M.lookup fft delegates of
    Just (Method _ _ fn _ _) ->
      return fn
    _ -> do
      let fn = "delegate" ++ show (M.size delegates)
      let ForeignFunctionType{..} = fft
      let invocation = ldarg 0 : concatMap (\arg -> loadArg arg ++ [apply0]) (zip [1..] parameterTypes)
      let f = delegateFunction [MaAssembly] returnType fn parameterTypes returnTypeIO invocation
      put $ st { delegates = M.insert fft f delegates }
      return fn
  where apply0 = call [] Cil.Object "" moduleName "APPLY0" [Cil.Object, Cil.Object]

cilTypeOf :: PrimitiveType -> CilCodegen ()
cilTypeOf t = tell [ ldtoken t
                   , call [] runtimeType "mscorlib" "System.Type" "GetTypeFromHandle" [runtimeTypeHandle] ]

delegateFunction :: [MethAttr] -> PrimitiveType -> MethodName -> [PrimitiveType] -> Bool -> [MethodDecl] -> MethodDef
delegateFunction attrs retType fn paramTypes io invocation = Method attrs retType fn parameters body
  where parameters = zipWith param [(0 :: Int)..] paramTypes
        param i t  = Param Nothing t ("p" ++ show i)
        body       = if io
                        then loadNothing : dup : invocation ++ [runIO, popBoxOrCast, ret]
                        else invocation ++ [popBoxOrCast, ret]
        runIO      = call [] Cil.Object "" moduleName "call__IO" [Cil.Object, Cil.Object, Cil.Object]
        popBoxOrCast = case retType of
                         Void -> pop
                         -- Exported data types are encoded as structs with a single `ptr` field
                         ValueType "" exportedDataType -> newobj "" exportedDataType [Cil.Object]
                         t | isValueType t -> unbox_any t
                         t -> castclass t


-- Exported data types are encoded as structs with a single `ptr` field
loadArg :: (Int, PrimitiveType) -> [MethodDecl]
loadArg (i, ValueType "" exportedDataType) = [ ldarga i
                                             , ldfld Cil.Object "" exportedDataType "ptr" ]
loadArg (i, t) = ldarg i : [box t | isValueType t]

castOrUnbox :: PrimitiveType -> CilCodegen ()
castOrUnbox t =
  tell [
    if isValueType t
       then unbox_any t
       else castclass t
    ]

isValueType :: PrimitiveType -> Bool
isValueType (ValueType _ _) = True
isValueType Float32 = True
isValueType Int32   = True
isValueType Bool    = True
isValueType Char    = True
isValueType _       = False

loadSConTag :: CilCodegen ()
loadSConTag = tell [ castclass (ReferenceType "" "SCon")
                   , ldfld Int32 "" "SCon" "tag" ]

cgIfThenElse :: LVar -> SExp -> SExp -> (String -> CilCodegen ()) -> CilCodegen ()
cgIfThenElse v thenAlt elseAlt cgBranch = do
  thenLabel <- gensym "THEN"
  endLabel  <- gensym "END"
  load v
  cgBranch thenLabel
  cil elseAlt
  tell [ br endLabel
       , label thenLabel ]
  cil thenAlt
  tell [ label endLabel ]

cgConst :: Const -> CilCodegen ()
cgConst (Str s) = tell [ ldstr s ]
cgConst (I i)   = cgConst . BI . fromIntegral $ i
cgConst (BI i)  = tell [ ldc i
                       , boxInt32 ]
cgConst (Ch c)  = tell [ ldc $ ord c
                       , boxChar ]
cgConst (Fl d)  = tell [ ldc_r4 (double2Float d)
                       , boxFloat32 ]
cgConst c = unsupported "const" c
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

cgCase :: Bool -> LVar -> [SAlt] -> CilCodegen ()
cgCase check v alts@(SConstCase{} : _) = cgSwitchCase check v alts loadTag altTag
  where loadTag = tell [ unbox_any Int32 ]
        altTag (SConstCase (I t) _) = t
        altTag alt = error $ "expecting (SConstCase (I t)) got: " ++ show alt

cgCase check v alts = cgSwitchCase check v alts loadTag altTag
  where loadTag = loadSConTag
        altTag (SConCase _ t _ _ _) = t
        altTag alt = error $ "expecting SConCase got: " ++ show alt

cgSwitchCase :: Bool -> LVar -> [SAlt] -> CilCodegen () -> (SAlt -> Int) -> CilCodegen ()
cgSwitchCase check val alts loadTag altTag | canBuildJumpTable alts = do
  labelPrefix <- gensym "L"
  let labels = map ((labelPrefix++) . show) [0..(length alts - 1)]
  endLabel <- gensym "END"
  load val
  loadTag
  tell [ ldc baseTag
       , sub ]

  when check $ do
    -- when caseIndex < 0 goto defaultCase
    caseIndex <- gensym "caseIndex"
    tell [ localsInit [ Local Int32 caseIndex ]
         , stlocN caseIndex
         , ldlocN caseIndex
         , ldc_i4 0
         , blt (last labels)
         , ldlocN caseIndex ]

  tell [ switch labels ]
  mapM_ (cgAlt endLabel val) (zip labels alts)
  tell [ label endLabel ]
  where canBuildJumpTable (a:as) = canBuildJumpTable' (altTag a) as
        canBuildJumpTable _      = False
        canBuildJumpTable' _ [SDefaultCase _]     = True
        canBuildJumpTable' t (a:as) | t' == t + 1 = canBuildJumpTable' t' as where t' = altTag a
        canBuildJumpTable' _ _                    = False
        baseTag = altTag (head alts)
cgSwitchCase _ _ _ _ alts = unsupported "switch case alternatives" alts

cgAlt :: Label -> LVar -> (Label, SAlt) -> CilCodegen ()
cgAlt end v (l, alt) = do
  tell [ label l ]
  cg alt
  tell [ br end ]
  where cg (SConstCase _ e) = cil e
        cg (SDefaultCase e) = cil e
        cg c                = cgSConCase v c

storeLocal :: Int -> CilCodegen ()
storeLocal i = do
  tell [ stloc i ]
  modify ensureLocal
  where ensureLocal st@CodegenState{..} = st { localCount = max localCount (i + 1) }

cgBranchEq :: Const -> String -> CilCodegen ()
cgBranchEq (BI i) target = cgBranchEq (I . fromIntegral $ i) target
cgBranchEq (Ch c) target =
  tell [ unbox_any Char
       , ldc $ ord c
       , beq target ]
cgBranchEq (I i) target =
  tell [ unbox_any Int32
       , ldc i
       , beq target ]
cgBranchEq c _ = unsupported "branch on const" c

cgSConCase :: LVar -> SAlt -> CilCodegen ()
cgSConCase v (SConCase offset _ _ fs sexp) = do
  unless (null fs) $ do
    load v
    tell [ castclass sconTypeRef
         , ldfld array "" "SCon" "fields" ]
    offset' <- localIndex offset
    mapM_ project (zip [0..length fs - 1] [offset'..])
    tell [ pop ]
  cil sexp
  where project (f, l) = do tell [ dup
                                 , ldc f
                                 , ldelem_ref ]
                            storeLocal l
cgSConCase _ c = unsupported "SConCase" c

cgOp :: PrimFn -> [LVar] -> CilCodegen ()
cgOp LWriteStr [_, s] = do
  load s
  tell [ castclass String
       , call [] Void "mscorlib" "System.Console" "Write" [String]
       , loadNothing ]

cgOp LReadStr [_] =
  tell [ call [] String "mscorlib" "System.Console" "ReadLine" [] ]

cgOp LStrRev [s] = do
  loadString s
  tell [ callvirt charArray "mscorlib" "System.String" "ToCharArray" []
       , dup
       , call [] Void "mscorlib" "System.Array" "Reverse" [ReferenceType "mscorlib" "System.Array"]
       , newobj "mscorlib" "System.String" [charArray] ]

cgOp LStrLen [s] = do
  loadString s
  tell [ callvirt Int32 "mscorlib" "System.String" "get_Length" []
       , boxInt32 ]

cgOp LStrConcat args = do
  forM_ args loadString
  tell [ call [] String "mscorlib" "System.String" "Concat" (map (const String) args) ]

cgOp LStrCons [h, t] = do
  loadAs Char h
  tell [ call [] String "mscorlib" "System.Char" "ToString" [Char] ]
  loadString t
  tell [ call [] String "mscorlib" "System.String" "Concat" [String, String] ]

cgOp LStrSubstr [index, count, s] = do
  loadString s
  loadAs Int32 index
  loadAs Int32 count
  tell [ callvirt String "mscorlib" "System.String" "Substring" [Int32, Int32] ]

cgOp LStrEq args = do
  forM_ args loadString
  tell [ call [] Bool "mscorlib" "System.String" "op_Equality" (map (const String) args)
       , boxInt32 ] -- strange but correct

cgOp LStrHead [v] = do
  loadString v
  tell [ ldc_i4 0
       , call [CcInstance] Char "mscorlib" "System.String" "get_Chars" [Int32]
       , boxChar ]

cgOp LStrTail [v] = do
  loadString v
  tell [ ldc_i4 1
       , call [CcInstance] String "mscorlib" "System.String" "Substring" [Int32] ]

cgOp (LStrInt ITNative) [v] = do
  val <- gensym "val"
  tell [ localsInit [ Local Int32 val ]
       , ldc_i4 0
       , stlocN val ]
  loadString v
  tell [ ldlocaN val
       , call [] Bool "mscorlib" "System.Int32" "TryParse" [String, ByRef Int32]
       , pop
       , ldlocN val
       , boxInt32 ]

cgOp (LStrInt i) [_] = unsupported "LStrInt" i

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
cgOp (LExternal nul)        [] | nul == sUN "prim__null" = tell [ ldnull ]
cgOp o _ = unsupported "operation" o

primitiveToString :: LVar -> CilCodegen ()
primitiveToString p = do load p
                         tell [ objectToString ]

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
  st@(CodegenState suffix _ _) <- get
  put $ st { nextSuffix = suffix + 1 }
  return $ prefix ++ show suffix

intOp :: MethodDecl -> [LVar] -> CilCodegen ()
intOp = numOp Int32

floatOp :: MethodDecl -> [LVar] -> CilCodegen ()
floatOp = numOp Float32

numOp :: PrimitiveType -> MethodDecl -> [LVar] -> CilCodegen ()
numOp t = primitiveOp t t

primitiveOp :: PrimitiveType -> PrimitiveType -> MethodDecl -> [LVar] -> CilCodegen ()
primitiveOp argT resT op args = do
  forM_ args (loadAs argT)
  tell [ op
       , box resT ]

convert :: PrimitiveType -> PrimitiveType -> String -> LVar -> CilCodegen ()
convert from to fn arg = do
  loadAs from arg
  tell [ call [] to "mscorlib" "System.Convert" fn [from]
       , box to ]

boxInt32, boxFloat32, boxChar, boxBoolean :: MethodDecl
boxInt32   = box Int32
boxFloat32 = box Float32
boxChar    = box Char
boxBoolean = box Bool

loadAs :: PrimitiveType -> LVar -> CilCodegen ()
loadAs valueType l = do
  load l
  tell [ unbox_any valueType ]

loadString :: LVar -> CilCodegen ()
loadString l = do
  load l
  tell [ castclass String ]

ldc :: (Integral n) => n -> MethodDecl
ldc = ldc_i4 . fromIntegral

load :: LVar -> CilCodegen ()
load (Loc i) = do
  li <- localIndex i
  tell [
    if li < 0
      then ldarg i
      else ldloc li ]
load v = unsupported "LVar" v

localIndex :: Offset -> CilCodegen Offset
localIndex i = do
  (CodegenInput _ paramCount) <- ask
  return $ i - paramCount

entryPointName :: Name
entryPointName = MN 0 "runMain"
--entryPointName = NS (UN "main") ["Main"]

cilName :: Name -> String
cilName = quoted . T.unpack . showName

showName :: Name -> T.Text
showName (NS n ns) = T.intercalate "." . reverse $ showName n : ns
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

sconType :: [MethodDef] -> TypeDef
sconType methods = classDef [CaPrivate] className noExtends noImplements
                            [sconTag, sconFields] allMethods []
  where className  = "SCon"
        sconTag    = Field [FaPublic, FaInitOnly] Int32 "tag"
        sconFields = Field [FaPublic, FaInitOnly] array "fields"
        allMethods = [sconCtor, sconToString] ++ methods
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
