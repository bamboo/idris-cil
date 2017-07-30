{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}
module IRTS.CodegenCil (codegenCil, codegenCilTrans, compileCilCodegenInfo, CilCodegenInfo) where

import           IRTS.Cil.Builders
import           IRTS.Cil.CaseDispatch
import           IRTS.Cil.FFI
import           IRTS.Cil.MaxStack
import           IRTS.Cil.OptimizeLocals
import           IRTS.Cil.Parsers (parseAssemblyRef)
import           IRTS.Cil.Types

import           IRTS.CodegenCommon
import           IRTS.Compiler
import           IRTS.Lang
import           IRTS.Simplified
import           Idris.AbsSyntax (Idris, IState(idris_patdefs), getIState, IRFormat(IBCFormat), Codegen(Via))
import           Idris.Core.CaseTree (CaseType(Shared))
import           Idris.Core.TT hiding (Impossible)
import           Idris.ElabDecls (elabPrims, elabMain)
import           Idris.Main (loadInputs)

import           Language.Cil
import qualified Language.Cil as Cil

import           Control.Arrow ((&&&))
import           Control.Monad.RWS.Strict hiding (local)
import           Control.Monad.State.Strict

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Char (ord)
import           Data.DList (DList, empty, singleton, fromList, toList, snoc)
import qualified Data.IntSet as IntSet
import           Data.List (partition)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           System.FilePath (takeBaseName, takeExtension, replaceExtension)
import           System.Process (readProcess)

type CilCodegenInfo = (CodegenInfo, IState)

data CilCodegenState = CilCodegenState
  { delegateTypes :: !(M.Map ForeignFunctionType MethodDef)
  , cafs          :: !CAFs
  , constTags     :: !IntSet.IntSet
  , recordArities :: !IntSet.IntSet }

type CAFs = M.Map Name TypeName

type CilCodegen = State CilCodegenState

compileCilCodegenInfo :: [FilePath] -> FilePath -> Idris CilCodegenInfo
compileCilCodegenInfo inputs output = do
  elabPrims
  _ <- loadInputs inputs Nothing
  mainProg <- elabMain
  ci <- compile (Via IBCFormat "cil") output (Just mainProg)
  istate <- getIState
  pure (ci, istate)

codegenCilTrans :: (Assembly -> Assembly) -> CilCodegenInfo -> IO ()
codegenCilTrans assemblyTransformation cci@(ci, _) =
  do writeFileUTF8 cilFile cilText
     when (outputExtension /= ".il") $ do
       ilasm cilFile output
       writeFileUTF8 dotnetRuntimeConfigFile dotnetRuntimeConfig
  where cilFile = replaceExtension output "il"
        cilText = pr (assemblyTransformation . assemblyFor $ cci) ""
        output  = outputFile ci
        outputExtension = takeExtension output
        writeFileUTF8 f s = BS.writeFile f $ UTF8.fromString s
        dotnetRuntimeConfigFile = replaceExtension output ".runtimeconfig.json"
        dotnetRuntimeConfig = "{\n\
                              \  \"runtimeOptions\": {\n\
                              \    \"framework\": {\n\
                              \      \"name\": \"Microsoft.NETCore.App\",\n\
                              \      \"version\": \"1.1.0\"\n\
                              \    }\n\
                              \  }\n\
                              \}"

codegenCil :: CilCodegenInfo -> IO ()
codegenCil = codegenCilTrans id

ilasm :: String -> String -> IO ()
ilasm input output = readProcess "ilasm" [input, "-output=" <> output] "" >>= putStr

assemblyFor :: CilCodegenInfo -> Assembly
assemblyFor cci@(ci, _) = Assembly (mscorlibRef : libs) asmName types
  where asmName = quoted $ takeBaseName (outputFile ci)
        libs = parseAssemblyRef <$> compileLibs ci
        types = typesFor cci

typesFor :: CilCodegenInfo -> [TypeDef]
typesFor cci@(ci, _) =
  let (mainModule, CilCodegenState{..}) = runState (moduleFor ci) emptyCilCodegenState
      recordType' = recordType (M.elems delegateTypes) (IntSet.toList constTags)
      (exportedTypes', cafs') = runState (exportedTypes cci) cafs
      recordTypes = recordTypeFor <$> IntSet.toList recordArities
      types = mainModule : recordType' : nothingType : boxedBoolType : exportedTypes'
              ++ cafTypesFor (M.assocs cafs')
              ++ recordTypes
  in types

cafTypesFor :: [(Name, TypeName)] -> [TypeDef]
cafTypesFor = fmap (uncurry cafTypeFor)

emptyCilCodegenState :: CilCodegenState
emptyCilCodegenState = CilCodegenState M.empty M.empty IntSet.empty IntSet.empty

moduleFor :: CodegenInfo -> CilCodegen TypeDef
moduleFor ci = do methods <- mapM method declsWithBody
                  return $ privateSealedClass moduleName noExtends noImplements [] methods []
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
      body = optimizeBooleanBoxing . toList $
        if isEntryPoint
           then
             mconcat [ [entryPoint]
                     , locals lc
                     , fromList . removeLastTailCall . toList $ cilForSexp
                     , [pop, ret] ]
           else
             mconcat [ [comment (show decl)]
                     , optimizeLocals lc (toList (cilForSexp <> singleton ret)) ]
  put cilCodegenState'
  return $ cilMethod attrs retType (cilName name) parameters body
  where attrs      = [MaStatic, MaAssembly]
        retType    = if isEntryPoint then Cil.Void else Cil.Object
        parameters = param <$> ps
        param n    = Param Nothing Cil.Object (cilName n)
        locals lc  = fromList [localsInit $ local <$> [0..(lc - 1)] | lc > 0]
        local i    = Local Cil.Object ("l" <> show i)
        isEntryPoint = name == entryPointName
        removeLastTailCall :: [Instruction] -> [Instruction]
        removeLastTailCall [OpCode (Tailcall e), OpCode Ret, OpCode Ldnull] = [OpCode e]
        removeLastTailCall (x:xs) = x:removeLastTailCall xs
        removeLastTailCall _ = error "Entry point should end in tail call"

data CilExport
  = CilFun  { unCilFun :: !MethodDef }
  | CilType { unCilType :: !TypeDef }

type CAF a = State CAFs a

exportedTypes :: CilCodegenInfo -> CAF [TypeDef]
exportedTypes cci@(ci, _) = concatMapM exports (exportDecls ci)
  where exports :: ExportIFace -> CAF [TypeDef]
        exports (Export (sn -> "FFI_CIL") exportedDataType es) = do
          cilExports <- mapM (cilExport cci) es
          let (cilFuns, cilTypes) = partition isCilFun cilExports
              methods = unCilFun <$> cilFuns
              types   = unCilType <$> cilTypes
          pure $ publicClass exportedDataType methods : types
          where isCilFun (CilFun _) = True
                isCilFun _          = False
                publicClass name methods = publicSealedClass name noExtends noImplements [] methods []
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

cilExport :: CilCodegenInfo -> Export -> CAF CilExport
cilExport cci (ExportFun fn@(NS n _) desc rt ps) = do
  invocation <-
    if null ps
       then do instruction <- loadCAF fn
               pure [instruction]
       else pure (loadArgs <> [ app fn ps ])
  pure . CilFun $ delegateFunction [MaPublic, MaStatic] retType exportName parameters io invocation
  where retType    = foreignType rt
        exportName = case desc of
                       FApp (UN (T.unpack -> "CILExport")) (FStr alias:_) -> alias
                       _ -> cilName n
        parameters = zip paramTypes (maybe paramNames (<> paramNames) (originalParameterNamesOf fn cci))
        paramTypes = foreignType <$> ps
        loadArgs   = zip [0..] paramTypes >>= loadArg
        io         = isIO rt

cilExport _ (ExportData (FStr exportedDataType)) = pure . CilType $ publicStruct exportedDataType [ptr] [ctor] []
  where ptr  = Field [FaAssembly, FaInitOnly] Cil.Object "ptr"
        ctor = Constructor [MaAssembly] Void [Param Nothing Cil.Object "ptr"]
                 [ ldarg 0
                 , ldarg 1
                 , stfld Cil.Object "" exportedDataType "ptr"
                 , ret ]

cilExport _ e = error $ "invalid export: " <> show e


data SimpleDeclaration = SimpleDeclaration !SDecl !Int -- cached param count

type MethodBody = DList Instruction

data CilEmitterState = CilEmitterState
  { nextSuffix :: !Int
  , localCount :: !Int
  , cilCodegenState  :: !CilCodegenState }

type CilEmitter a = RWS SimpleDeclaration MethodBody CilEmitterState a

insertConstTag :: Int -> CilEmitterState -> CilEmitterState
insertConstTag tag ces@(CilEmitterState _ _ cgs@CilCodegenState{..}) =
  ces { cilCodegenState = cgs { constTags = IntSet.insert tag constTags } }

insertRecordArity :: Int -> CilEmitterState -> CilEmitterState
insertRecordArity arity ces@CilEmitterState { cilCodegenState = cgs } =
  ces { cilCodegenState = cgs { recordArities = IntSet.insert arity (recordArities cgs) } }

ensureRecordTypeFor :: Int -> CilEmitter PrimitiveType
ensureRecordTypeFor arity = do
  modify (insertRecordArity arity)
  pure (ReferenceType "" (recordTypeNameFor arity))

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

-- Special constructors: Nothing, Just a
emit (SCon _ 0 n [])  | n == maybeNothing = tell [ ldnull ]
emit (SCon _ 1 n [v]) | n == maybeJust    = load v

-- General constructors
emit (SCon Nothing t _ fs) =
  if null fs
    then do modify (insertConstTag t)
            tell [ ldsfld recordTypeRef "" recordTypeName (constRecordFieldNameForTag t) ]
    else do tell [ ldc t ]
            mapM_ load fs
            let arity = length fs
            tell [ newobj "" (recordTypeNameFor arity) (Int32 : replicate arity Cil.Object) ]

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


-- Prelude.Maybe pattern matching
emit (SCase Shared v [ SConCase local 1 nJust [_] justAlt
                     , SConCase _ 0 nNothing [] nothingAlt ])
  | nJust == maybeJust && nNothing == maybeNothing = emitCaseJust v local justAlt nothingAlt

emit (SCase Shared v [ SConCase local 1 nJust [_] justAlt
                     , SDefaultCase defaultAlt ])
  | nJust == maybeJust = emitCaseJust v local justAlt defaultAlt

emit (SCase Shared v [ SConCase _ 0 nNothing [] nothingAlt
                     , SDefaultCase elseAlt ])
  | nNothing == maybeNothing = emitIfThenElse v nothingAlt elseAlt (\thenLabel -> tell [ brfalse thenLabel ])

emit c@(SCase Shared v (SConCase _ _ n1 a1 alt1 : _))
  | n1 == maybeNothing || n1 == maybeJust = unsupported "maybe pattern" c


-- arbitrary pattern matching
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

emit (SCase Shared v alts@(SConstCase (Ch _) _ : _)) = emitSwitchCase v alts (const loadTag) altTag
  where
    loadTag = tell [ unbox_any Char
                   , conv_i4 ]
    altTag (SConstCase (Ch t) _) = ord t
    altTag alt = error $ "expecting (SConstCase (Ch t)) got: " <> show alt

emit (SCase Shared v alts@(SConstCase (I _) _ : _)) = emitSwitchCase v alts (const loadTag) altTag
  where
    loadTag = tell [ unbox_any Int32 ]
    altTag (SConstCase (I t) _) = t
    altTag alt = error $ "expecting (SConstCase (I t)) got: " <> show alt

emit e@(SCase Shared v alts) = emitSwitchCase v (withDefaultCase alts) (const loadRecordTag) tagFromSConCase
  where
    withDefaultCase :: [SAlt] -> [SAlt]
    withDefaultCase alts =
      if any isDefaultCase alts
        then alts
        else alts <> [SDefaultCase SNothing]

emit (SChkCase _ [SDefaultCase e]) = emit e

emit (SChkCase v alts) = emitSwitchCase v alts maybeLoadRecordTag tagFromSConCase
  where
    maybeLoadRecordTag :: String -> CilEmitter ()
    maybeLoadRecordTag defaultLabel = do
      tell [ isinst recordTypeName ]
      record <- storeTempFromStack recordTypeRef
      tell [ ldlocN record
           , brfalse defaultLabel
           , ldlocN record
           , loadRecordTagField ]


emit (SApp isTailCall n args) =
  if null args
    then do instruction <- liftCAFOperation $ loadCAF n
            tell [ instruction ]
    else do forM_ args load
            if isTailCall
              then tell [ tailcall app', ret, ldnull ]
              else tell [ app' ]
  where app' = app n args

emit (SForeign retDesc desc args) = emitForeign $ parseDescriptor desc
  where emitForeign :: CILForeign -> CilEmitter ()

        emitForeign (CILDelegate t) =
          cilDelegate t retDesc args

        emitForeign (CILTypeOf t) =
          cilTypeOf t

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
delegateFunction attrs retType fn ps io invocation = cilMethod attrs retType fn parameters body
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

cilMethod :: [MethAttr] -> PrimitiveType -> MethodName -> [Parameter] -> [Instruction] -> MethodDef
cilMethod attrs retType name parameters body = Method attrs retType name parameters (withMaxStack body retType)
  where
    withMaxStack :: [Instruction] -> PrimitiveType -> [Instruction]
    withMaxStack body retType = maxStack (maxStackFor body retType) : body

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

loadRecordTag :: CilEmitter ()
loadRecordTag = tell [ castclass recordTypeRef
                     , loadRecordTagField ]

loadRecordTagField :: Instruction
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

emitCaseJust :: LVar -> Int -> SExp -> SExp -> CilEmitter ()
emitCaseJust v justValueLocal justAlt nothingAlt = do
  justLabel <- gensym "JUST"
  endLabel  <- gensym "END"
  load v
  tell [ brtrue justLabel ]
  emit nothingAlt
  tell [ br endLabel
       , label justLabel ]
  load v
  localIndex justValueLocal >>= storeLocal
  emit justAlt
  tell [ label endLabel ]

emitConst :: Const -> CilEmitter ()
emitConst (Str s) = tell [ ldstr s ]
emitConst (I i)   = emitConst . BI . fromIntegral $ i
emitConst (B32 i) = emitConst . BI . fromIntegral $ i
emitConst (B16 i) = emitConst . BI . fromIntegral $ i
emitConst (B8 i)  = emitConst . BI . fromIntegral $ i
emitConst (BI i)  = tell [ ldc i
                         , boxInt32 ]
emitConst (Ch c)  = tell [ ldc $ ord c
                         , boxChar ]
emitConst (Fl d)  = tell [ ldc_r8 d
                         , boxDouble64 ]
emitConst c       = unsupported "const" c
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

tagFromSConCase :: SAlt -> Int
tagFromSConCase (SConCase _ t _ _ _) = t
tagFromSConCase alt = error $ "expecting SConCase got: " <> show alt

uniqueLabelsFor :: [a] -> CilEmitter [String]
uniqueLabelsFor alts = do
  uniqueLabelPrefix <- gensym "L"
  pure $ (uniqueLabelPrefix <>) . show <$> [0..(length alts - 1)]

isDefaultCase :: SAlt -> Bool
isDefaultCase SDefaultCase{} = True
isDefaultCase _              = False

emitSwitchCase :: LVar -> [SAlt] -> (String -> CilEmitter ()) -> (SAlt -> Int) -> CilEmitter ()
emitSwitchCase var alts loadTagFromStack altTag =
  let
    (caseAlts, [SDefaultCase defaultSExp]) = Prelude.break isDefaultCase alts
    taggedCaseAlts = (altTag &&& id) <$> caseAlts
  in
    dispatchUsing (dispatchStrategyFor taggedCaseAlts) defaultSExp
  where
    dispatchUsing :: DispatchStrategy SAlt -> SExp -> CilEmitter ()
    dispatchUsing (JumpTable entries) defaultSExp = do
      (defaultLabel, endLabel, entryLabels) <- prepareToDispatchOn entries
      let baseTag = fst . head $ entries
      adjustTagOffset baseTag
      tell [ switch entryLabels ]
      tell [ label defaultLabel ]
      emit defaultSExp
      tell [ br endLabel ]
      zipWithM_ (emitJumpEntry defaultLabel endLabel) entryLabels (snd <$> entries)
      tell [ label endLabel ]

    dispatchUsing (LinearSearch entries) defaultSExp = do
      (defaultLabel, endLabel, entryLabels) <- prepareToDispatchOn entries
      tagVar <- storeTempFromStack Int32
      zipWithM_ (emitLinearSearchEntry tagVar endLabel) entries (Prelude.tail entryLabels <> [defaultLabel])
      emit defaultSExp
      tell [ label endLabel ]

    prepareToDispatchOn :: [a] -> CilEmitter (Label, Label, [Label])
    prepareToDispatchOn entries = do
      defaultLabel <- gensym "DEFAULT"
      endLabel <- gensym "END"
      entryLabels <- uniqueLabelsFor entries
      loadTagOrJumpTo defaultLabel
      pure (defaultLabel, endLabel, entryLabels)

    adjustTagOffset :: Int -> CilEmitter ()
    adjustTagOffset baseTag =
      when (baseTag /= 0) $
        tell [ ldc baseTag
             , sub ]

    emitJumpEntry :: Label -> Label -> Label -> JumpTableEntry SAlt -> CilEmitter ()
    emitJumpEntry defaultLabel _        l DefaultEntry =
      tell [ label l
           , br defaultLabel ]
    emitJumpEntry _            endLabel l (Entry alt) = do
      tell [ label l ]
      emitAlt alt
      tell [ br endLabel ]

    emitLinearSearchEntry :: String -> Label -> (Int, SAlt) -> Label -> CilEmitter ()
    emitLinearSearchEntry var endLabel (tag, alt) nextLabel = do
      tell [ ldlocN var ]
      tell $
        if tag == 0
            then [ brtrue nextLabel ]
            else [ ldc tag
                 , ceq
                 , brfalse nextLabel ]
      emitAlt alt
      tell [ br endLabel
           , label nextLabel ]

    emitAlt (SConstCase _ e) = emit e
    emitAlt (SDefaultCase e) = emit e
    emitAlt c                = emitSConCase var c

    loadTagOrJumpTo :: Label -> CilEmitter ()
    loadTagOrJumpTo defaultLabel = load var >> loadTagFromStack defaultLabel


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
emitBranchEq (Ch c)  target = emitPrimitiveBranchEq Char  (ldc (ord c)) target
emitBranchEq (B32 i) target = emitPrimitiveBranchEq Int32 (ldc i) target
emitBranchEq (I i)   target = emitPrimitiveBranchEq Int32 (ldc i) target
emitBranchEq (BI i)  target = emitPrimitiveBranchEq Int32 (ldc i) target
emitBranchEq c       _      = unsupported "branch on const" c

emitPrimitiveBranchEq :: PrimitiveType -> Instruction -> String -> CilEmitter ()
emitPrimitiveBranchEq ty ldconst target =
  tell [ unbox_any ty
       , ldconst
       , beq target ]

emitSConCase :: LVar -> SAlt -> CilEmitter ()
emitSConCase v (SConCase offset _ _ fs sexp) = do
  unless (null fs) $ do
    load v
    recordType <- ensureRecordTypeFor arity
    tell [ castclass recordType ]
    offset' <- localIndex offset
    zipWithM_ (project recordType) (recordFieldNamesFor arity) [offset'..]
    tell [ pop ]
  emit sexp
  where arity = length fs
        project (ReferenceType assembly typeName) fn l = do
          tell [ dup
               , ldfld Cil.Object assembly typeName fn ]
          storeLocal l
emitSConCase _ c = unsupported "SConCase" c

emitOp :: PrimFn -> [LVar] -> CilEmitter ()
emitOp (LSHL (ITFixed IT32)) args = emitInt32Op shl args
emitOp (LLSHR (ITFixed IT32)) args = emitInt32Op shr_un args
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

emitOp (LXOr (ITFixed IT32)) args = emitInt32Op Cil.xor args
emitOp (LOr (ITFixed IT32)) args = emitInt32Op Cil.or args
emitOp (LAnd (ITFixed IT32)) args = emitInt32Op Cil.and args
emitOp (LAnd (ITFixed IT8)) args = emitInt32Op Cil.and args
emitOp (LTrunc (ITFixed IT32) (ITFixed IT16)) [x] = load x
emitOp (LTrunc (ITFixed IT32) (ITFixed IT8))  [x] = load x
emitOp (LTrunc (ITFixed IT16) (ITFixed IT8))  [x] = load x
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
emitOp (LTrunc ITBig ITNative) [i] = load i
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
emitOp (LEq ATFloat)          args = emitPrimitiveOp Double64 Int32 ceq args
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
emitPrimitiveOp argTy resTy op args = do
  forM_ args (loadAs argTy)
  tell [ op
       , box resTy ]

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
loadNothing = ldsfld Cil.Object "" "Nothing" nothingFieldName

nothingFieldName :: FieldName
nothingFieldName = "Value"

nothingType :: TypeDef
nothingType = privateSealedClass className noExtends noImplements
                    [nothing] [defaultCtorDef, cctor] []
  where className = "Nothing"
        nothing   = Field [FaStatic, FaPublic, FaInitOnly] Cil.Object nothingFieldName
        cctor     = Constructor [MaStatic] Void []
                      [ newobj "" className []
                      , stsfld Cil.Object "" className nothingFieldName
                      , ret ]

liftCAFOperation :: CAF a -> CilEmitter a
liftCAFOperation op = do
  ces <- get
  let cgs = cilCodegenState ces
      (v, cafs') = runState op (cafs cgs)
  put ces { cilCodegenState = cgs { cafs = cafs' } }
  pure v

loadCAF :: Name -> CAF Instruction
loadCAF n = do
  cafTypeName <- cafTypeNameFor n
  pure $ ldsfld Cil.Object "" cafTypeName cafFieldName

cafTypeNameFor :: Name -> CAF TypeName
cafTypeNameFor n = do
  cafs <- get
  case M.lookup n cafs of
    Just typeName ->
      pure typeName
    Nothing       ->
      let typeName = "CAF" ++ show (length cafs)
      in do put $ M.insert n typeName cafs
            pure typeName

cafFieldName :: FieldName
cafFieldName = "Value"

cafTypeFor :: Name -> TypeName -> TypeDef
cafTypeFor caf className = privateSealedClass className noExtends noImplements allFields allMethods []
  where tag        = Field [FaStatic, FaPublic, FaInitOnly] Cil.Object cafFieldName
        allFields  = [tag]
        allMethods = [cctor, defaultCtorDef]
        cctor      = Constructor [MaStatic] Void []
                      [ app caf []
                      , stsfld Cil.Object "" className cafFieldName
                      , ret ]

recordType :: [MethodDef] -> [Int] -> TypeDef
recordType methods constTags = classDef [CaPrivate, CaBeforeFieldInit] className noExtends noImplements allFields allMethods []
  where className  = recordTypeName
        allFields  = tag : constFields
        tag        = Field [FaPublic, FaInitOnly] Int32 "tag"
        constFields = (Field [FaStatic, FaPublic, FaInitOnly] recordTypeRef . constRecordFieldNameForTag) <$> constTags
        allMethods = [cctor, ctor, toString] <> methods
        ctor       = Constructor [MaPublic] Void [ Param Nothing Int32 "tag" ]
                       [ ldarg 0
                       , call [CcInstance] Void "" "object" ".ctor" []
                       , ldarg 0
                       , ldarg 1
                       , stfld Int32 "" className "tag"
                       , ret ]
        toString   = Method [MaPublic, MaVirtual] String "ToString" []
                       [ ldstr (className <> " ")
                       , ldarg 0
                       , ldfld Int32 "" className "tag"
                       , boxInt32
                       , objectToString
                       , call [] String "mscorlib" "System.String" "Concat" [String, String]
                       , ret ]
        cctor      = Constructor [MaStatic] Void [] $
                      concatMap constFieldInitializer constTags
                      <> [ ret ]

        constFieldInitializer :: Int -> [Instruction]
        constFieldInitializer tag =
          [ ldc_i4 (fromIntegral tag)
          , newobj "" recordTypeName [Int32]
          , stsfld recordTypeRef "" className (constRecordFieldNameForTag tag) ]

constRecordFieldNameForTag :: Int -> String
constRecordFieldNameForTag = ("R"++) . show

recordTypeRef :: PrimitiveType
recordTypeRef = ReferenceType "" recordTypeName

recordTypeName :: String
recordTypeName = "Record"

recordTypeFor :: Int -> TypeDef
recordTypeFor arity = privateSealedClass className baseType noImplements allFields allMethods []
  where className  = recordTypeNameFor arity
        baseType   = Just (TypeSpec recordTypeName)
        fieldNames = recordFieldNamesFor arity
        allFields  = Field [FaPublic, FaInitOnly] Cil.Object <$> fieldNames
        allMethods = [ctor]
        ctor       = Constructor [MaPublic] Void (Param Nothing Int32 "tag" : (Param Nothing Cil.Object <$> fieldNames)) $
                       [ ldarg 0
                       , ldarg 1
                       , call [CcInstance] Void "" recordTypeName ".ctor" [Int32] ]
                       <> storeFields
                       <> [ ret ]
        storeFields :: [Instruction]
        storeFields = concat (zipWith storeField fieldNames [2..])
        storeField :: FieldName -> Int -> [Instruction]
        storeField f a = [ ldarg 0
                         , ldarg a
                         , stfld Cil.Object "" className f ]

recordFieldNamesFor :: Int -> [FieldName]
recordFieldNamesFor arity = ("f" ++) . show <$> [1..arity]

recordTypeNameFor :: Int -> TypeName
recordTypeNameFor = (recordTypeName ++) . show

boxedBoolTypeName :: TypeName
boxedBoolTypeName = "BoxedBool"

boxedBoolType :: TypeDef
boxedBoolType = privateSealedClass className noExtends noImplements allFields allMethods []
  where className  = boxedBoolTypeName
        allFields  = constFields
        constFields = Field [FaStatic, FaPublic, FaInitOnly] Cil.Object <$> ["False", "True"]
        allMethods = [defaultCtorDef, cctor, for]
        cctor      = Constructor [MaStatic] Void []
                       [ ldc_i4 0
                       , boxBoolean
                       , stsfld Cil.Object "" className "False"
                       , ldc_i4 1
                       , boxBoolean
                       , stsfld Cil.Object "" className "True"
                       , ret ]
        for        = Method [MaStatic] Cil.Object "For" [Param Nothing Bool "b"]
                       [ ldarg 0
                       , brtrue "TRUE"
                       , ldBoxedFalse
                       , ret
                       , label "TRUE"
                       , ldBoxedTrue
                       , ret ]

ldBoxedFalse, ldBoxedTrue :: Instruction
ldBoxedFalse = ldsfld Cil.Object "" boxedBoolTypeName "False"
ldBoxedTrue  = ldsfld Cil.Object "" boxedBoolTypeName "True"

optimizeBooleanBoxing :: [Instruction] -> [Instruction]
optimizeBooleanBoxing = go empty
  where
    go acc (OpCode Ldc_i4_0 : OpCode (Box Bool) : xs) = go (snoc acc ldBoxedFalse) xs
    go acc (OpCode Ldc_i4_1 : OpCode (Box Bool) : xs) = go (snoc acc ldBoxedTrue) xs
    go acc (OpCode (Box Bool) : xs) = go (snoc acc boxedBoolFor) xs
    go acc (x : xs) = go (snoc acc x) xs
    go acc []       = toList acc
    boxedBoolFor = call [] Cil.Object "" boxedBoolTypeName "For" [Bool]

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

maybeNothing, maybeJust :: Name
maybeNothing = NS (UN "Nothing") ["Maybe", "Prelude"]
maybeJust    = NS (UN "Just")  ["Maybe", "Prelude"]

quoted :: String -> String
quoted name = "'" <> (name >>= validChar) <> "'"
  where validChar :: Char -> String
        validChar c = if c == '\''
                         then "\\'"
                         else [c]

app :: Name -> [a] -> Instruction
app n args = call [] Cil.Object "" moduleName (cilName n) (const Cil.Object <$> args)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM a b = concat <$> mapM a b
