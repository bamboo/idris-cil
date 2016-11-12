{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}
module IRTS.CodegenCil (codegenCil, compileCilCodegenInfo, CilCodegenInfo) where

import           Control.Monad.RWS.Strict hiding (local)
import           Control.Monad.State.Strict

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Char (ord)
import           Data.DList (DList, empty, singleton, fromList, toList)
import           Data.Function (on)
import           Data.List (partition, sortBy)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text as T

import           IRTS.Cil.FFI
import           IRTS.Cil.MaxStack
import           IRTS.CodegenCommon
import           IRTS.Compiler
import           IRTS.Lang
import           IRTS.Simplified
import           Idris.AbsSyntax (Idris, IState(idris_patdefs), getIState, IRFormat(IBCFormat), Codegen(Via))
import           Idris.Core.CaseTree (CaseType(Shared))
import           Idris.Core.TT
import           Idris.ElabDecls (elabPrims, elabMain)
import           Idris.Main (loadInputs)

import           Language.Cil
import qualified Language.Cil as Cil

import           System.FilePath (takeBaseName, takeExtension, replaceExtension)
import           System.Process (readProcess)

import           GHC.Float


-- |A CIL instruction.
type Instruction = MethodDecl

type CilCodegenInfo = (CodegenInfo, IState)

data CilCodegenState = CilCodegenState { delegateTypes :: M.Map ForeignFunctionType MethodDef
                                       , assemblyRefs  :: !AssemblyRefSet  }

type AssemblyRefSet = Set.Set AssemblyRef

type CilCodegen = State CilCodegenState

compileCilCodegenInfo :: [FilePath] -> FilePath -> Idris CilCodegenInfo
compileCilCodegenInfo inputs output = do
  elabPrims
  _ <- loadInputs inputs Nothing
  mainProg <- elabMain
  ci <- compile (Via IBCFormat "cil") output (Just mainProg)
  istate <- getIState
  pure (ci, istate)

codegenCil :: CilCodegenInfo -> IO ()
codegenCil cci@(ci, istate) =
  do writeFileUTF8 cilFile cilText
     when (outputExtension /= ".il") $
       ilasm cilFile output
  where cilFile = replaceExtension output "il"
        cilText = pr (assemblyFor cci) ""
        output  = outputFile ci
        outputExtension = takeExtension output
        writeFileUTF8 f s = BS.writeFile f $ UTF8.fromString s

ilasm :: String -> String -> IO ()
ilasm input output = readProcess "ilasm" [input, "/output:" <> output] "" >>= putStr

assemblyFor :: CilCodegenInfo -> Assembly
assemblyFor cci@(ci, _) = Assembly (mscorlibRef : assemblyRefs) asmName types
  where asmName = quoted $ takeBaseName (outputFile ci)
        (types, assemblyRefs) = typesFor cci

typesFor :: CilCodegenInfo -> ([TypeDef], [AssemblyRef])
typesFor cci@(ci, _) =
  let (mainModule, CilCodegenState{..}) = runState (moduleFor ci) (CilCodegenState M.empty Set.empty)
      types = mainModule : recordType (M.elems delegateTypes) : nothingType : exportedTypes cci
  in (types, Set.toList assemblyRefs)

moduleFor :: CodegenInfo -> CilCodegen TypeDef
moduleFor ci = do methods <- mapM method declsWithBody
                  return $ classDef [CaPrivate] moduleName noExtends noImplements [] methods []
  where declsWithBody = filter hasBody decls
        decls         = snd <$> simpleDecls ci
        hasBody (SFun _ _ _ SNothing) = False
        hasBody _                     = True

moduleName :: String
moduleName = "'λΠ'"

method :: SDecl -> CilCodegen MethodDef
method decl@(SFun name ps _ sexp) = do
  cilCodegenState <- get
  let (CilEmitterState _ lc cilCodegenState', cilForSexp) = cilFor cilCodegenState decl sexp
      body = if isEntryPoint
             then
               mconcat [ [entryPoint]
                       , locals lc
                       , fromList (removeLastTailCall $ toList cilForSexp)
                       , [pop, ret] ]
             else
               mconcat [ [comment (show decl)]
                       , locals lc
                       , cilForSexp
                       , [ret] ]
  put cilCodegenState'
  return $ Method attrs retType (cilName name) parameters (withMaxStack (toList body))
  where attrs      = [MaStatic, MaAssembly]
        retType    = if isEntryPoint then Cil.Void else Cil.Object
        parameters = param <$> ps
        param n    = Param Nothing Cil.Object (cilName n)
        locals lc  = fromList [localsInit $ local <$> [0..(lc - 1)] | lc > 0]
        local i    = Local Cil.Object ("l" <> show i)
        isEntryPoint = name == entryPointName
        withMaxStack body = maxStack (maxStackFor body retType) : body
        removeLastTailCall :: [Instruction] -> [Instruction]
        removeLastTailCall [OpCode (Tailcall e), OpCode Ret, OpCode Ldnull] = [OpCode e]
        removeLastTailCall (x:xs) = x:removeLastTailCall xs
        removeLastTailCall _ = error "Entry point should end in tail call"

data CilExport = CilFun  !MethodDef
               | CilType !TypeDef

exportedTypes :: CilCodegenInfo -> [TypeDef]
exportedTypes cci@(ci, _) = exportDecls ci >>= exports
  where exports :: ExportIFace -> [TypeDef]
        exports (Export (sn -> "FFI_CIL") exportedDataType es) =
            let cilExports = cilExport cci <$> es
                (cilFuns, cilTypes) = partition isCilFun cilExports
                methods = (\(CilFun m) -> m) <$> cilFuns
                types   = (\(CilType t) -> t) <$> cilTypes
            in publicClass exportedDataType methods : types
          where isCilFun (CilFun _) = True
                isCilFun _          = False
                publicClass name methods = classDef [CaPublic] name noExtends noImplements [] methods []
        exports e = error $ "Unsupported Export: " <> show e

-- |Queries the Idris state for the parameter names of the first function definition with the given name.
originalParameterNamesOf :: Name -> CilCodegenInfo -> Maybe [String]
originalParameterNamesOf fn@(NS n _) (_, istate) = do
  -- idris_patdefs is like an inverted index of all top-level pattern definitions
  --     SimpleName -> Map FQN Definition
  let patDefsBySimpleName = idris_patdefs istate
  patDefsByName <- M.lookup n patDefsBySimpleName
  ((paramStack, _, _) : _, _) <- M.lookup fn patDefsByName
  pure (cilName . fst <$> reverse paramStack)

cilExport :: CilCodegenInfo -> Export -> CilExport
cilExport cci (ExportFun fn@(NS n _) desc rt ps) = CilFun f
  where f          = delegateFunction [MaPublic, MaStatic] retType exportName parameters io invocation
        retType    = foreignType rt
        exportName = case desc of
                       FApp (UN (T.unpack -> "CILExport")) (FStr alias:_) -> alias
                       _ -> cilName n
        parameters = zip paramTypes (maybe paramNames (<> paramNames) (originalParameterNamesOf fn cci))
        paramTypes = foreignType <$> ps
        invocation = loadArgs <> [ call [] Cil.Object "" moduleName (cilName fn) (const Cil.Object <$> ps) ]
        loadArgs   = zip [0..] paramTypes >>= loadArg
        io         = isIO rt

cilExport _ (ExportData (FStr exportedDataType)) = CilType $ publicStruct exportedDataType [ptr] [ctor] []
  where ptr  = Field [FaAssembly, FaInitOnly] Cil.Object "ptr"
        ctor = Constructor [MaAssembly] Void [Param Nothing Cil.Object "ptr"]
                 [ ldarg 0
                 , ldarg 1
                 , stfld Cil.Object "" exportedDataType "ptr"
                 , ret ]

cilExport _ e = error $ "invalid export: " <> show e


data SimpleDeclaration = SimpleDeclaration !SDecl !Int -- cached param count

type MethodBody = DList Instruction

data CilEmitterState = CilEmitterState { nextSuffix :: !Int
                                       , localCount :: !Int
                                       , cilCodegenState  :: !CilCodegenState }

type CilEmitter a = RWS SimpleDeclaration MethodBody CilEmitterState a

insertAssemblyRef :: AssemblyRef -> CilEmitterState -> CilEmitterState
insertAssemblyRef assemblyRef ces@(CilEmitterState _ _ cgs@(CilCodegenState _ assemblyRefs)) =
  ces { cilCodegenState = cgs { assemblyRefs = Set.insert assemblyRef assemblyRefs } }

cilFor :: CilCodegenState -> SDecl -> SExp -> (CilEmitterState, MethodBody)
cilFor cilCodegenState decl@(SFun _ params _ _) sexp =
  execRWS (emit sexp)
          (SimpleDeclaration decl (length params))
          (CilEmitterState 0 0 cilCodegenState)

emit :: SExp -> CilEmitter ()
emit (SLet (Loc i) v e) = do
  case v of
    SNothing -> tell [ loadNothing ]
    _        -> emit v
  localIndex i >>= storeLocal
  emit e

emit (SUpdate _ v) = emit v
emit (SV v)        = load v
emit (SConst c)    = emitConst c
emit (SOp op args) = emitOp op args
emit SNothing      = emitThrow "SNothing"
emit (SError e)    = emitThrow e

-- Special constructors: True, False
emit (SCon _ 0 n []) | n == boolFalse = tell [ ldc_i4 0, boxBoolean ]
emit (SCon _ 1 n []) | n == boolTrue  = tell [ ldc_i4 1, boxBoolean ]

-- General constructors
emit (SCon Nothing t _ fs) = do
  tell [ ldc t ]
  if null fs
    then tell [ loadNoFields ]
    else do tell [ ldc $ length fs
                 , newarr Cil.Object ]
            mapM_ storeElement (zip [0..] fs)
  tell [ newobj "" recordTypeName [Int32, array] ]
  where storeElement (i, f) = do
          tell [ dup
               , ldc_i4 i ]
          load f
          tell [ stelem_ref ]

-- ifThenElse
emit (SCase Shared v [ SConCase _ 0 nFalse [] elseAlt
                     , SConCase _ 1 nTrue  [] thenAlt ]) | nFalse == boolFalse && nTrue == boolTrue =
  emitIfThenElse v thenAlt elseAlt $
    \thenLabel -> tell [ unbox_any Bool
                       , brtrue thenLabel ]

emit (SCase Shared v [ SConCase _ 0 nFalse [] elseAlt
                     , SDefaultCase thenAlt ]) | nFalse == boolFalse =
  emitIfThenElse v thenAlt elseAlt $
    \thenLabel -> tell [ unbox_any Bool
                       , brtrue thenLabel ]

emit (SCase Shared v [ SConCase _ tag _ [] thenAlt, SDefaultCase elseAlt ]) =
  emitIfThenElse v thenAlt elseAlt $
    \thenLabel -> do loadRecordTag
                     tell [ ldc tag
                          , beq thenLabel ]

-- In some situations idris gives us a SCase with two default clauses
emit (SCase Shared v [t@SConstCase{}, e@SDefaultCase{}, SDefaultCase{}]) = emit (SCase Shared v [t, e])

emit (SCase Shared v [SConstCase c thenAlt, SDefaultCase elseAlt]) =
  emitIfThenElse v thenAlt elseAlt $ \thenLabel ->
    emitBranchEq c thenLabel

emit (SCase Shared v [c@SConCase{}]) = emitSConCase v c

emit (SCase Shared v alts@(SConstCase (Ch _) _ : _)) = do
  val <- storeTemp Char v
  labels <- uniqueLabelsFor alts
  emitLabeledAlts (emitAlt val) alts labels
  where
    emitAlt :: String -> Label -> (String, SAlt) -> CilEmitter ()
    emitAlt val end (l, SConstCase (Ch t) e) = do
      tell [ ldc $ ord t
           , ldlocN val
           , ceq
           , brfalse l ]
      emit e
      tell [ br end
           , label l ]
    emitAlt _ end (l, SDefaultCase e) = do
      emit e
      tell [ br end ]
    emitAlt _ _ (_, c) = unsupported "char case" c


emit e@(SCase Shared v alts) = let (cases, defaultCase) = partition caseType alts
                              in case defaultCase of
                                   [] -> emitCase v (sorted cases <> [SDefaultCase SNothing])
                                   _  -> emitCase v (sorted cases <> defaultCase)
   where sorted = sortBy (compare `on` tag)
         tag (SConCase _ t _ _ _) = t
         tag (SConstCase (I t) _) = t
         tag c                    = unsupportedCase c
         caseType SDefaultCase{}  = False
         caseType _               = True
         unsupportedCase c        = error $ show c <> " in\n" <> show e

emit (SChkCase _ [SDefaultCase e]) = emit e
emit (SChkCase v alts) = emitChkCase v alts

emit (SApp isTailCall n args) = do
  forM_ args load
  if isTailCall
    then tell [ tailcall app, ret, ldnull ]
    else tell [ app ]
  where app = call [] Cil.Object "" moduleName (cilName n) (const Cil.Object <$> args)

emit (SForeign retDesc desc args) = emitForeign $ parseDescriptor desc
  where emitForeign :: CILForeign -> CilEmitter ()

        emitForeign (CILAssemblyRef assemblyName version pubKeyToken) = do
          modify (insertAssemblyRef (AssemblyRef assemblyName version pubKeyToken))
          tell [ loadNothing ]

        emitForeign (CILDelegate t) =
          cilDelegate t retDesc args

        emitForeign (CILTypeOf t) =
          cilTypeOf t

        emitForeign (CILEnumValueOf t i) =
          tell [ ldc i
               , box t ]

        emitForeign CILConstructor =
          case retType of
            Array _ -> emitNewArray retType
            _       -> emitNewInstance

        emitForeign (CILInstance fn) = do
          let declType : paramTypes = sig
          case declType of
            Array _ -> emitArrayFFI declType fn
            _       -> emitInstanceFFI declType fn paramTypes

        emitForeign ffi = do
          loadArgs
          case ffi of
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
            CILCall method ->
              tell [ callMethod method ]
            _ -> error $ "unsupported ffi descriptor: " <> show ffi
          acceptBoxOrPush retType

        emitNewArray (Array elTy) = do
          loadArgs
          tell [ newarr elTy ]

        emitArrayFFI (Array elTy@ValueType{}) "set_Item" = do
          let [ array, index, value ] = typedArgs
          loadTypedArg array
          loadTypedArg index
          tell [ ldelema elTy ]
          loadTypedArg value
          tell [ stobj elTy
               , loadNothing ]

        emitArrayFFI (Array elTy) fn = do
          loadArgs
          case fn of
            "get_Length" -> tell [ ldlen ]
            "get_Item"   -> tell $ ldelemFor elTy
            "set_Item"   -> tell [ stelemFor elTy ]
            op           -> unsupported "array FFI" op
          acceptBoxOrPush retType

        ldelemFor :: PrimitiveType -> MethodBody
        ldelemFor Int32 = [ ldelem_i4 ]
        ldelemFor ty@ValueType{} = [ ldelema ty
                                   , ldobj ty ]

        ldelemFor String = [ ldelem_ref ]
        ldelemFor Cil.Object = [ ldelem_ref ]
        ldelemFor ty@ReferenceType{} = [ ldelem_ref ]
        ldelemFor ty    = error $ "No ldelem for " ++ show ty

        stelemFor String = stelem_ref
        stelemFor Cil.Object = stelem_ref
        stelemFor ty@ReferenceType{} = stelem_ref
        stelemFor Int32 = stelem_i4
        stelemFor ty    = error $ "No stelem for " ++ show ty

        emitNewInstance = do
          loadArgs
          let (assemblyName, typeName) = assemblyNameAndTypeFrom retType
          tell [ newobj   assemblyName typeName sig ]
          acceptBoxOrPush retType

        emitInstanceFFI declType fn paramTypes = do
          let (assemblyName, typeName) = assemblyNameAndTypeFrom declType
          if isValueType declType
             then do
               let (receiverType, receiverArg) : effectiveArgs = typedArgs
               receiver <- storeTemp receiverType receiverArg
               tell [ ldlocaN receiver ]
               mapM_ loadTypedArg effectiveArgs
               tell [ call [CcInstance] retType assemblyName typeName fn paramTypes ]
             else do
               loadArgs
               tell [ callvirt retType assemblyName typeName fn paramTypes ]
          acceptBoxOrPush retType

        loadArgs = mapM_ loadTypedArg typedArgs
        typedArgs = zip sig (snd <$> args)

        loadTypedArg :: (PrimitiveType, LVar) -> CilEmitter ()
        loadTypedArg (t, loc) = do
          load loc
          castOrUnbox t

        acceptBoxOrPush :: PrimitiveType -> CilEmitter ()
        acceptBoxOrPush Void              = tell [ loadNothing ]
        acceptBoxOrPush t | isValueType t = tell [ box t ]
        acceptBoxOrPush _                 = return ()
        sig                               = foreignType . fst <$> args
        retType                           = foreignType retDesc

emit e = unsupported "expression" e

-- Delegates are emitted as instance functions of the general Record data type
-- so we can avoid the overhead of an additional closure object at runtime
cilDelegate :: PrimitiveType -> FDesc -> [(FDesc, LVar)] -> CilEmitter ()
cilDelegate delegateTy retDesc [(_, fnArg)] = do
  let fft = parseForeignFunctionType retDesc
  fn <- delegateMethodFor fft
  load fnArg
  let (delegateAsm, delegateTyName) = assemblyNameAndTypeFrom delegateTy
  let ForeignFunctionType{..} = fft
  tell [ castclass recordTypeRef
       , ldftn_instance returnType "" recordTypeName fn parameterTypes
       , newobj delegateAsm delegateTyName [Cil.Object, IntPtr] ]
cilDelegate _ retDesc _ = unsupported "delegate" retDesc

delegateMethodFor :: ForeignFunctionType -> CilEmitter String
delegateMethodFor fft = do
  st@(CilEmitterState _ _ cgs@CilCodegenState{..}) <- get
  case M.lookup fft delegateTypes of
    Just (Method _ _ fn _ _) ->
      return fn
    _ -> do
      let fn = "delegate" <> show (M.size delegateTypes)
      let ForeignFunctionType{..} = fft
      let invocation = ldarg 0 : (zip [1..] parameterTypes >>= (<> [apply0]) . loadArg)
      let parameters = zip parameterTypes paramNames
      let f = delegateFunction [MaAssembly] returnType fn parameters returnTypeIO invocation
      put $ st { cilCodegenState = cgs { delegateTypes = M.insert fft f delegateTypes } }
      return fn
  where apply0 = call [] Cil.Object "" moduleName "APPLY0" [Cil.Object, Cil.Object]

cilTypeOf :: PrimitiveType -> CilEmitter ()
cilTypeOf t = tell [ ldtoken t
                   , call [] runtimeType "mscorlib" "System.Type" "GetTypeFromHandle" [runtimeTypeHandle] ]

delegateFunction :: [MethAttr] -> PrimitiveType -> MethodName -> [(PrimitiveType, ParamName)] -> Bool -> [Instruction] -> MethodDef
delegateFunction attrs retType fn ps io invocation = Method attrs retType fn parameters body
  where parameters = uncurry (Param Nothing) <$> ps
        body       = if io
                        then loadNothing : dup : invocation <> [runIO, popBoxOrCast, ret]
                        else invocation <> [popBoxOrCast, ret]
        runIO      = call [] Cil.Object "" moduleName "call__IO" [Cil.Object, Cil.Object, Cil.Object]
        popBoxOrCast = case retType of
                         Void -> pop
                         -- Exported data types are encoded as structs with a single `ptr` field
                         ValueType "" exportedDataType -> newobj "" exportedDataType [Cil.Object]
                         t | isValueType t -> unbox_any t
                         t -> castclass t

paramNames :: [String]
paramNames = (("p" <>) . show) <$> [0..]

-- Exported data types are encoded as structs with a single `ptr` field
loadArg :: (Int, PrimitiveType) -> [Instruction]
loadArg (i, ValueType "" exportedDataType) = [ ldarga i
                                             , ldfld Cil.Object "" exportedDataType "ptr" ]
loadArg (i, t) = ldarg i : [box t | isValueType t]

castOrUnbox :: PrimitiveType -> CilEmitter ()
castOrUnbox t =
  tell [
    if isValueType t
       then unbox_any t
       else castclass t
    ]

isValueType :: PrimitiveType -> Bool
isValueType (ValueType _ _) = True
isValueType Double64 = True
isValueType Float32 = True
isValueType Int32   = True
isValueType Bool    = True
isValueType Char    = True
isValueType _       = False

loadRecordTag :: CilEmitter ()
loadRecordTag = tell [ castclass recordTypeRef
                     , loadRecordTagField ]

loadRecordTagField = ldfld Int32 "" recordTypeName "tag"

emitIfThenElse :: LVar -> SExp -> SExp -> (String -> CilEmitter ()) -> CilEmitter ()
emitIfThenElse v thenAlt elseAlt emitBranch = do
  thenLabel <- gensym "THEN"
  endLabel  <- gensym "END"
  load v
  emitBranch thenLabel
  emit elseAlt
  tell [ br endLabel
       , label thenLabel ]
  emit thenAlt
  tell [ label endLabel ]

emitConst :: Const -> CilEmitter ()
emitConst (Str s) = tell [ ldstr s ]
emitConst (I i)   = emitConst . BI . fromIntegral $ i
emitConst (B32 i) = emitConst . BI . fromIntegral $ i
emitConst (B16 i) = emitConst . BI . fromIntegral $ i
emitConst (B8 i) = emitConst . BI . fromIntegral $ i
emitConst (BI i)  = tell [ ldc i
                         , boxInt32 ]
emitConst (Ch c)  = tell [ ldc $ ord c
                         , boxChar ]
emitConst (Fl d)  = tell [ ldc_r8 d
                         , boxDouble64 ]
emitConst c = unsupported "const" c
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

emitCase :: LVar -> [SAlt] -> CilEmitter ()
emitCase v alts@(SConstCase (I _) _ : _) = emitSwitchCase v alts (const loadTag) altTag
  where loadTag = tell [ unbox_any Int32 ]
        altTag (SConstCase (I t) _) = t
        altTag alt = error $ "expecting (SConstCase (I t)) got: " <> show alt

emitCase v alts = emitSwitchCase v (consecutiveAltsFor alts) (const loadRecordTag) tagFromSConCase

emitChkCase :: LVar -> [SAlt] -> CilEmitter ()
emitChkCase v alts = emitSwitchCase v (consecutiveAltsFor alts) maybeLoadRecordTag tagFromSConCase
  where maybeLoadRecordTag :: String -> CilEmitter ()
        maybeLoadRecordTag defaultLabel = do
          tell [ isinst recordTypeName ]
          record <- storeTempFromStack recordTypeRef
          tell [ ldlocN record
               , brfalse defaultLabel
               , ldlocN record
               , loadRecordTagField
               ]

consecutiveAltsFor :: [SAlt] -> [SAlt]
consecutiveAltsFor alts = let (caseAlts, defaultAlts) = span isSConCase alts
                          in fillInTheGaps tagFromSConCase unreachableAlt caseAlts ++ defaultAlts
  where unreachableAlt tag = SConCase 0 tag unreachableName [] SNothing

tagFromSConCase :: SAlt -> Int
tagFromSConCase (SConCase _ t _ _ _) = t
tagFromSConCase alt = error $ "expecting SConCase got: " <> show alt

isSConCase :: SAlt -> Bool
isSConCase SConCase{} = True
isSConCase _          = False

unreachableName :: Name
unreachableName = UN "unreachable!"

fillInTheGaps :: (a -> Int) -> (Int -> a) -> [a] -> [a]
fillInTheGaps extract create = go empty
  where go acc (i:j:rest) = let gap = [(extract i + 1)..(extract j - 1)]
                            in go (acc <> singleton i <> fromList (create <$> gap)) (j:rest)
        go acc rest       = toList acc <> rest

uniqueLabelsFor :: [a] -> CilEmitter [String]
uniqueLabelsFor alts = do
  uniqueLabelPrefix <- gensym "L"
  pure $ (uniqueLabelPrefix <>) . show <$> [0..(length alts - 1)]

emitLabeledAlts :: (Label -> (Label, SAlt) -> CilEmitter ()) -> [SAlt] -> [Label] -> CilEmitter ()
emitLabeledAlts emitAlt alts labels = do
  endLabel <- gensym "END"
  mapM_ (emitAlt endLabel) (zip labels alts)
  tell [ label endLabel ]

emitSwitchCase :: LVar -> [SAlt] -> (String -> CilEmitter ()) -> (SAlt -> Int) -> CilEmitter ()
emitSwitchCase val alts loadTag altTag | canBuildJumpTable alts = do
  labels <- uniqueLabelsFor alts
  let defaultLabel = last labels
  load val
  loadTag defaultLabel
  tell [ ldc baseTag
       , sub
       , switch labels
       , br defaultLabel ]
  emitLabeledAlts (emitAlt val) alts labels
  where canBuildJumpTable (a:as) = canBuildJumpTable' (altTag a) as
        canBuildJumpTable _      = False
        canBuildJumpTable' _ [SDefaultCase _]     = True
        canBuildJumpTable' t (a:as) | t' == t + 1 = canBuildJumpTable' t' as where t' = altTag a
        canBuildJumpTable' _ _                    = False
        baseTag = altTag (head alts)
        emitAlt v end (l, alt) = do
          tell [ label l ]
          case alt of
            SConstCase _ e -> emit e
            SDefaultCase e -> emit e
            c              -> emitSConCase v c
          tell [ br end ]

emitSwitchCase _ alts _ _ = unsupported "switch case alternatives" (descAlt <$> alts)

descAlt :: SAlt -> String
descAlt (SConCase _ t _ _ _) = "SConCase " <> show t
descAlt (SConstCase t _) = "SConstCase " <> show t
descAlt (SDefaultCase _) = "SDefaultCase"

storeLocal :: Int -> CilEmitter ()
storeLocal i = do
  tell [ stloc i ]
  modify ensureLocal
  where ensureLocal st@CilEmitterState{..} = st { localCount = max localCount (i + 1) }

emitBranchEq :: Const -> String -> CilEmitter ()
emitBranchEq (BI i) target = emitBranchEq (I . fromIntegral $ i) target
emitBranchEq (Ch c) target =
  tell [ unbox_any Char
       , ldc $ ord c
       , beq target ]
emitBranchEq (I i) target =
  tell [ unbox_any Int32
       , ldc i
       , beq target ]
emitBranchEq c _ = unsupported "branch on const" c

emitSConCase :: LVar -> SAlt -> CilEmitter ()
emitSConCase v (SConCase offset _ _ fs sexp) = do
  unless (null fs) $ do
    load v
    tell [ castclass recordTypeRef
         , ldfld array "" recordTypeName "fields" ]
    offset' <- localIndex offset
    mapM_ project (zip [0..length fs - 1] [offset'..])
    tell [ pop ]
  emit sexp
  where project (f, l) = do tell [ dup
                                 , ldc f
                                 , ldelem_ref ]
                            storeLocal l
emitSConCase _ c = unsupported "SConCase" c

emitOp :: PrimFn -> [LVar] -> CilEmitter ()
emitOp (LLSHR (ITFixed IT32)) args = emitInt32Op shr args
emitOp (LLSHR (ITFixed IT16)) [x, y] = do
  loadAs Int32 x
  tell [ conv_u2 ]
  loadAs Int32 y
  tell [ ldc_i4 0x1f
       , Cil.and
       , shr
       , boxInt32 ]

emitOp (LLSHR (ITFixed IT8)) [x, y] = do
  loadAs Int32 x
  tell [ conv_u1 ]
  loadAs Int32 y
  tell [ ldc_i4 0x1f
       , Cil.and
       , shr
       , boxInt32 ]

emitOp (LAnd (ITFixed IT32)) args = emitInt32Op Cil.and args
emitOp (LAnd (ITFixed IT8)) args = emitInt32Op Cil.and args
emitOp (LTrunc (ITFixed IT32) (ITFixed IT16)) [x] = load x
emitOp (LTrunc (ITFixed IT16) (ITFixed IT8)) [x] = load x
emitOp LWriteStr [_, s] = do
  load s
  tell [ castclass String
       , call [] Void "mscorlib" "System.Console" "Write" [String]
       , loadNothing ]

emitOp LReadStr [_] =
  tell [ call [] String "mscorlib" "System.Console" "ReadLine" [] ]

emitOp LStrRev [s] = do
  loadString s
  tell [ callvirt charArray "mscorlib" "System.String" "ToCharArray" []
       , dup
       , call [] Void "mscorlib" "System.Array" "Reverse" [systemArray]
       , newobj "mscorlib" "System.String" [charArray] ]

emitOp LStrLen [s] = do
  loadString s
  tell [ callvirt Int32 "mscorlib" "System.String" "get_Length" []
       , boxInt32 ]

emitOp LStrConcat args = do
  forM_ args loadString
  tell [ call [] String "mscorlib" "System.String" "Concat" (const String <$> args) ]

emitOp LStrCons [h, t] = do
  loadAs Char h
  tell [ call [] String "mscorlib" "System.Char" "ToString" [Char] ]
  loadString t
  tell [ call [] String "mscorlib" "System.String" "Concat" [String, String] ]

emitOp LStrSubstr [index, count, s] = do
  indexOutOfRange <- gensym "indexOutOfRange"
  countInRange <- gensym "countInRange"
  end <- gensym "end"
  indexVar  <- storeTemp Int32 index
  stringVar <- storeTemp String s
  lengthVar <- gensym "length"
  tell [ ldlocN stringVar
       , callvirt Int32 "mscorlib" "System.String" "get_Length" []
       , localsInit [ Local Int32 lengthVar ]
       , stlocN lengthVar
       , ldlocN indexVar
       , ldlocN lengthVar
       , bge indexOutOfRange ]
  countVar <- storeTemp Int32 count
  tell [ ldlocN countVar
       , ldlocN indexVar
       , add
       , ldlocN lengthVar
       , ble countInRange
       , ldlocN lengthVar
       , ldlocN indexVar
       , sub
       , stlocN countVar
       , label countInRange
       , ldlocN stringVar
       , ldlocN indexVar
       , ldlocN countVar
       , callvirt String "mscorlib" "System.String" "Substring" [Int32, Int32]
       , br end
       , label indexOutOfRange
       , ldstr ""
       , label end ]

emitOp LStrEq args = do
  forM_ args loadString
  tell [ call [] Bool "mscorlib" "System.String" "op_Equality" (const String <$> args)
       , boxInt32 ]

emitOp LStrLt args = do
  forM_ args loadString
  tell [ call [CcInstance] Int32 "mscorlib" "System.String" "CompareTo" [String]
       , ldc_i4 0
       , clt
       , boxInt32 ]

emitOp LStrHead [v] = do
  loadString v
  tell [ ldc_i4 0
       , call [CcInstance] Char "mscorlib" "System.String" "get_Chars" [Int32]
       , boxChar ]

emitOp LStrTail [v] = do
  loadString v
  tell [ ldc_i4 1
       , call [CcInstance] String "mscorlib" "System.String" "Substring" [Int32] ]

emitOp LStrIndex [s, i] = do
  loadString s
  loadAs Int32 i
  tell [ callvirt Char "mscorlib" "System.String" "get_Chars" [Int32]
       , boxChar ]

emitOp (LStrInt ITNative) [s] = emitTryParse Int32 s

emitOp (LStrInt i) [_] = unsupported "LStrInt" i

emitOp (LChInt ITNative) [c] = do
  load c
  tell [ unbox_any Char
       , boxInt32 ]

emitOp (LSExt ITNative ITBig) [i]  = load i
emitOp (LZExt ITNative ITBig) [i]  = load i
emitOp (LZExt ITNative (ITFixed IT32)) [i] = load i
emitOp (LZExt (ITFixed IT32) ITNative) [i] = load i
emitOp (LZExt (ITFixed IT16) ITNative) [i] = load i
emitOp (LZExt (ITFixed IT8) ITNative) [i]  = load i
emitOp (LPlus (ATInt ITChar)) args = emitPrimitiveOp Char Char add args
emitOp (LPlus (ATInt _))      args = emitInt32Op add args
emitOp (LMinus (ATInt ITChar))args = emitPrimitiveOp Char Char sub args
emitOp (LMinus (ATInt _))     args = emitInt32Op sub args
emitOp (LTimes (ATInt _))     args = emitInt32Op mul args
emitOp (LSRem (ATInt _))      args = emitInt32Op Cil.rem args
emitOp (LEq (ATInt ITChar))   args = emitPrimitiveOp Char Int32 ceq args
emitOp (LEq (ATInt _))        args = emitInt32Op ceq args
emitOp (LSLt (ATInt ITChar))  args = emitPrimitiveOp Char Int32 clt args
emitOp (LSLt (ATInt _))       args = emitInt32Op clt args
emitOp (LIntStr _)            [i]  = emitPrimitiveToString i
emitOp (LIntFloat _)          [i]  = emitPrimitiveCast Int32 Double64 conv_r8 i
emitOp (LIntCh _)             [i]  = emitPrimitiveCast Int32 Char conv_i2 i
emitOp (LTimes ATFloat)       args = emitFloatOp mul args
emitOp (LSDiv ATFloat)        args = emitFloatOp Cil.div args
emitOp (LPlus ATFloat)        args = emitFloatOp add args
emitOp (LMinus ATFloat)       args = emitFloatOp sub args
emitOp LFloatStr              [f]  = emitPrimitiveToString f
emitOp LStrFloat              [s]  = emitTryParse Double64 s
emitOp (LExternal name)       args = emitExternalOp name args
emitOp o _ = unsupportedOp o

emitExternalOp :: Name -> [LVar] -> CilEmitter()
emitExternalOp name []
  | name == sUN "prim__null" = tell [ ldnull ]
emitExternalOp name [x, y]
  | name == sUN "prim__eqPtr" = load x >> load y >> tell [ ceq, boxInt32 ]
emitExternalOp (sn -> customExternalName) args = emitCustomExternalOp customExternalName args

emitCustomExternalOp :: String -> [LVar] -> CilEmitter ()
emitCustomExternalOp "prim__singleFromDouble" [x]  = emitPrimitiveCast Double64 Float32 conv_r4 x
emitCustomExternalOp "prim__singleFromInteger" [x] = emitPrimitiveCast Int32 Float32 conv_r4 x
emitCustomExternalOp "prim__singleFromInt" [x]     = emitPrimitiveCast Int32 Float32 conv_r4 x
emitCustomExternalOp "prim__singleAdd" args = emitSingleOp add args
emitCustomExternalOp "prim__singleSub" args = emitSingleOp sub args
emitCustomExternalOp "prim__singleMul" args = emitSingleOp mul args
emitCustomExternalOp "prim__singleDiv" args = emitSingleOp Cil.div args
emitCustomExternalOp "prim__singleCompare" [x, y] = do
  x' <- storeTemp Float32 x
  tell [ ldlocaN x' ]
  loadAs Float32 y
  tell [ call [CcInstance] Int32 "" "float32" "CompareTo" [Float32]
       , boxInt32 ]

emitCustomExternalOp "prim__singleMax" [x, y] = emitSingleMathOp "Max" x y
emitCustomExternalOp "prim__singleMin" [x, y] = emitSingleMathOp "Min" x y
emitCustomExternalOp "prim__singleNeg" [x] = do
  loadAs Float32 x
  tell [ neg
       , boxFloat32 ]

emitCustomExternalOp "prim__singleAbs" [x] = do
  loadAs Float32 x
  tell [ call [] Float32 "mscorlib" "System.Math" "Abs" [Float32]
       , boxFloat32 ]

emitCustomExternalOp "prim__singleShow" [x] = emitPrimitiveToString x

emitCustomExternalOp "prim__Vector_empty" [] =
  tell [ callMethod (GenericMethodInstance [] systemArray "Empty" [Cil.Object] [] (Array (GenericMethodTypeParameter 0))) ]

emitCustomExternalOp "prim__Vector_null" [v] = do
  loadAs array v
  tell [ ldlen
       , ldc_i4 0
       , ceq
       , boxBoolean ]

emitCustomExternalOp "prim__Vector_replaceAt" [n, x, v] = do
  loadAs array v
  tell [ callvirt Cil.Object "mscorlib" "System.Array" "Clone" []
       , castclass array
       , dup ]
  loadAs Int32 n
  load x
  tell [ stelem_ref ]

emitCustomExternalOp "prim__Vector_insertAt" [n, x, v] = do
  tempN <- storeTemp Int32 n
  let ldn = ldlocN tempN
  tempV <- storeTemp array v
  let ldv = ldlocN tempV
  result <- gensym "v"
  let ldr = ldlocN result
  tell [ localsInit [ Local array tempV
                    , Local array result ]
       , ldv
       , ldlen
       , conv_i4
       , ldc_i4 1
       , add
       , newarr Cil.Object
       , stlocN result
       , ldr
       , ldn ]
  load x
  tell [ stelem_ref
       , ldv
       , ldc_i4 0
       , ldr
       , ldc_i4 0
       , ldn
       , call [] Cil.Void "mscorlib" "System.Array" "Copy" [systemArray, Int32, systemArray, Int32, Int32]
       , ldv
       , ldn
       , ldr
       , ldn
       , ldc_i4 1
       , add
       , ldv
       , ldlen
       , conv_i4
       , ldn
       , sub
       , call [] Cil.Void "mscorlib" "System.Array" "Copy" [systemArray, Int32, systemArray, Int32, Int32]
       , ldr ]

emitCustomExternalOp "prim__Vector_replicate" [n, x] = do
  ldn <- storeTemp Int32 n
  ldx <- loadInstructionFor x
  v <- gensym "v"
  i <- gensym "i"
  loop <- gensym "LOOP"
  test <- gensym "TEST"
  tell [ localsInit [ Local array v
                    , Local Int32 i ]
       , ldlocN ldn
       , newarr Cil.Object
       , stlocN v
       , ldc_i4 0
       , stlocN i
       , br test
       , label loop
       , ldlocN v
       , ldlocN i
       , ldx
       , stelem_ref
       , ldlocN i
       , ldc_i4 1
       , add
       , stlocN i
       , label test
       , ldlocN i
       , ldlocN ldn
       , blt loop
       , ldlocN v ]

emitCustomExternalOp "prim__Vector_length" [v] = loadAs array v >> tell [ ldlen, boxInt32 ]

emitCustomExternalOp "prim__Vector_index" [v, i] = do
  loadAs array v
  loadAs Int32 i
  tell [ ldelem_ref ]

emitCustomExternalOp o _ = unsupported "external" o

emitTryParse :: PrimitiveType -> LVar -> CilEmitter ()
emitTryParse ty var = do
  let (asmName, tyName) = assemblyNameAndTypeFrom ty
  loadString var
  val <- gensym "val"
  tell [ localsInit [ Local ty val ] ]
  tell [ ldlocaN val
       , call [] Bool asmName tyName "TryParse" [String, ByRef ty]
       , pop
       , ldlocN val
       , box ty ]

emitSingleMathOp :: String -> LVar -> LVar -> CilEmitter ()
emitSingleMathOp op x y = do
  loadAs Float32 x
  loadAs Float32 y
  tell [ call [] Float32 "mscorlib" "System.Math" op [Float32, Float32]
       , boxFloat32 ]

emitSingleOp = emitPrimitiveOp Float32 Float32

emitPrimitiveCast :: PrimitiveType -> PrimitiveType -> Instruction -> LVar -> CilEmitter ()
emitPrimitiveCast from to inst var = do
  loadAs from var
  tell [ inst
       , box to ]

storeTemp :: PrimitiveType -> LVar -> CilEmitter String
storeTemp localType localVar = do
  loadAs localType localVar
  storeTempFromStack localType

storeTempFromStack :: PrimitiveType -> CilEmitter String
storeTempFromStack localType = do
  tempName <- gensym "temp"
  tell [ localsInit [ Local localType tempName ]
       , stlocN tempName ]
  pure tempName

unsupportedOp :: PrimFn -> CilEmitter ()
unsupportedOp = unsupported "operation"

emitPrimitiveToString :: LVar -> CilEmitter ()
emitPrimitiveToString p = load p >> tell [ objectToString ]

objectToString :: Instruction
objectToString = callvirt String "mscorlib" "System.Object" "ToString" []

unsupported :: Show a => String -> a -> CilEmitter ()
unsupported desc v = do
  (SimpleDeclaration decl _) <- ask
  emitThrow $ "Unsupported " <> desc <> " `" <> show v <> "' in\n" <> show decl

emitThrow :: String -> CilEmitter ()
emitThrow message =
  tell [ ldstr message
       , newobj "mscorlib" "System.Exception" [String]
       , throw
       , ldnull ]

gensym :: String -> CilEmitter String
gensym prefix = do
  st@(CilEmitterState suffix _ _) <- get
  put $ st { nextSuffix = suffix + 1 }
  return $ prefix <> show suffix

emitInt32Op :: Instruction -> [LVar] -> CilEmitter ()
emitInt32Op = emitNumOp Int32

emitFloatOp :: Instruction -> [LVar] -> CilEmitter ()
emitFloatOp = emitNumOp Double64

emitNumOp :: PrimitiveType -> Instruction -> [LVar] -> CilEmitter ()
emitNumOp t = emitPrimitiveOp t t

emitPrimitiveOp :: PrimitiveType -> PrimitiveType -> Instruction -> [LVar] -> CilEmitter ()
emitPrimitiveOp argT resT op args = do
  forM_ args (loadAs argT)
  tell [ op
       , box resT ]

boxDouble64, boxFloat32, boxInt32, boxChar, boxBoolean :: Instruction
boxDouble64 = box Double64
boxFloat32  = box Float32
boxInt32    = box Int32
boxChar     = box Char
boxBoolean  = box Bool

loadAs :: PrimitiveType -> LVar -> CilEmitter ()
loadAs valueType l = load l >> tell [ unbox_any valueType ]

loadString :: LVar -> CilEmitter ()
loadString l = load l >> tell [ castclass String ]

ldc :: (Integral n) => n -> Instruction
ldc = ldc_i4 . fromIntegral

loadInstructionFor :: LVar -> CilEmitter Instruction
loadInstructionFor (Loc i) = do
    li <- localIndex i
    if li < 0
      then pure $ ldarg i
      else pure $ ldloc li
loadInstructionFor v = error $ "Unsupported local variable: " ++ show v

load :: LVar -> CilEmitter ()
load v = do
  li <- loadInstructionFor v
  tell (singleton li)

localIndex :: Offset -> CilEmitter Offset
localIndex i = do
  (SimpleDeclaration _ paramCount) <- ask
  return $ i - paramCount

-- |Unpacks a simple name from a namespaced name.
sn :: Name -> String
sn (NS (UN n) _) = T.unpack n
sn _             = ""

entryPointName :: Name
entryPointName = MN 0 "runMain"

cilName :: Name -> String
cilName = quoted . T.unpack . showName

showName :: Name -> T.Text
showName (NS n ns) = T.intercalate "." . reverse $ showName n : ns
showName (UN t)    = t
showName (MN i t)  = T.concat [t, T.pack $ show i]
showName (SN sn)   = T.pack $ show sn
showName e = error $ "Unsupported name `" <> show e <> "'"

loadNothing :: Instruction
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

recordType :: [MethodDef] -> TypeDef
recordType methods = classDef [CaPrivate] className noExtends noImplements allFields allMethods []
  where className  = recordTypeName
        tag        = Field [FaPublic, FaInitOnly] Int32 "tag"
        fields     = Field [FaPublic, FaInitOnly] array "fields"
        allFields  = [tag, fields, noFields]
        allMethods = [cctor, ctor, toString] <> methods
        ctor       = Constructor [MaPublic] Void [ Param Nothing Int32 "tag"
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
        toString   = Method [MaPublic, MaVirtual] String "ToString" []
                       [ ldstr (className <> " ")
                       , ldarg 0
                       , ldfld Int32 "" className "tag"
                       , boxInt32
                       , objectToString
                       , call [] String "mscorlib" "System.String" "Concat" [String, String]
                       , ret ]
        noFields   = Field [FaStatic, FaPublic, FaInitOnly] array "NoFields"
        cctor      = Constructor [MaStatic] Void []
                      [ ldc_i4 0
                      , newarr Cil.Object
                      , stsfld array "" className "NoFields"
                      , ret ]

loadNoFields :: Instruction
loadNoFields = ldsfld array "" recordTypeName "NoFields"

recordTypeRef :: PrimitiveType
recordTypeRef = ReferenceType "" recordTypeName

recordTypeName :: String
recordTypeName = "Record"

publicStruct :: TypeName -> [FieldDef] -> [MethodDef] -> [TypeDef] -> TypeDef
publicStruct name = classDef [CaPublic] name (extends "[mscorlib]System.ValueType") noImplements

array, systemArray, charArray :: PrimitiveType
array = Array Cil.Object
systemArray = ReferenceType "mscorlib" "System.Array"
charArray = Array Char

runtimeType :: PrimitiveType
runtimeType = ReferenceType "mscorlib" "System.Type"

runtimeTypeHandle :: PrimitiveType
runtimeTypeHandle = ValueType "mscorlib" "System.RuntimeTypeHandle"

boolFalse, boolTrue :: Name
boolFalse = NS (UN "False") ["Bool", "Prelude"]
boolTrue  = NS (UN "True")  ["Bool", "Prelude"]

quoted :: String -> String
quoted name = "'" <> (name >>= validChar) <> "'"
  where validChar :: Char -> String
        validChar c = if c == '\''
                         then "\\'"
                         else [c]
