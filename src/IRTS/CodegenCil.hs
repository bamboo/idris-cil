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
import           Data.DList (DList, empty, singleton, fromList, toList)
import           Data.Function (on)
import           Data.List (partition, sortBy)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           IRTS.Cil.FFI
import           IRTS.Cil.MaxStack
import           IRTS.CodegenCommon
import           IRTS.Lang
import           IRTS.Simplified
import           Idris.Core.CaseTree (CaseType(Shared))
import           Idris.Core.TT

import           Language.Cil
import qualified Language.Cil as Cil

import           System.FilePath (takeBaseName, takeExtension, replaceExtension)
import           System.Process (readProcess)

import           GHC.Float


-- |A CIL instruction.
type Instruction = MethodDecl

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
ilasm input output = readProcess "ilasm" [input, "/output:" <> output] "" >>= putStr

type DelegateOutput = M.Map ForeignFunctionType MethodDef

type DelegateWriter = State DelegateOutput

assemblyFor :: CodegenInfo -> Assembly
assemblyFor ci = Assembly [mscorlibRef] asmName types
  where asmName = quoted $ takeBaseName (outputFile ci)
        types   = typesFor ci

typesFor :: CodegenInfo -> [TypeDef]
typesFor ci =
  let (mainModule, delegates) = runState (moduleFor ci) M.empty
  in mainModule : recordType (M.elems delegates) : nothingType : exportedTypes ci

moduleFor :: CodegenInfo -> DelegateWriter TypeDef
moduleFor ci = do methods <- mapM method declsWithBody
                  return $ classDef [CaPrivate] moduleName noExtends noImplements [] methods []
  where declsWithBody = filter hasBody decls
        decls         = snd <$> simpleDecls ci
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
               mconcat [ [entryPoint]
                       , locals lc
                       , fromList (removeLastTailCall $ toList cilForSexp)
                       , [pop, ret] ]
             else
               mconcat [ [comment (show decl)]
                       , locals lc
                       , cilForSexp
                       , [ret] ]
  put delegates'
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

exportedTypes :: CodegenInfo -> [TypeDef]
exportedTypes ci = exportDecls ci >>= exports
  where exports :: ExportIFace -> [TypeDef]
        exports (Export (sn -> "FFI_CIL") exportedDataType es) =
            let cilExports = cilExport <$> es
                (cilFuns, cilTypes) = partition isCilFun cilExports
                methods = (\(CilFun m) -> m) <$> cilFuns
                types   = (\(CilType t) -> t) <$> cilTypes
            in publicClass exportedDataType methods : types
          where isCilFun (CilFun _) = True
                isCilFun _          = False
                publicClass name methods = classDef [CaPublic] name noExtends noImplements [] methods []
        exports e = error $ "Unsupported Export: " <> show e

cilExport :: Export -> CilExport
cilExport (ExportFun fn@(NS n _) desc rt ps) = CilFun f
  where f          = delegateFunction [MaPublic, MaStatic] retType exportName paramTypes io invocation
        exportName = if null alias then cilName n else alias
        alias      = case desc of
                       FApp (UN (T.unpack -> "CILExport")) (FStr a:_) -> a
                       _ -> ""
        invocation = loadArgs <> [ call [] Cil.Object "" moduleName (cilName fn) (const Cil.Object <$> ps) ]
        loadArgs   = zip [0..] paramTypes >>= loadArg
        paramTypes = foreignType <$> ps
        retType    = foreignType rt
        io         = isIO rt

cilExport (ExportData (FStr exportedDataType)) = CilType $ publicStruct exportedDataType [ptr] [ctor] []
  where ptr  = Field [FaAssembly, FaInitOnly] Cil.Object "ptr"
        ctor = Constructor [MaAssembly] Void [Param Nothing Cil.Object "ptr"]
                 [ ldarg 0
                 , ldarg 1
                 , stfld Cil.Object "" exportedDataType "ptr"
                 , ret ]

cilExport e = error $ "invalid export: " <> show e


data CodegenInput = CodegenInput !SDecl !Int -- cached param count

data CodegenState = CodegenState { nextSuffix :: !Int
                                 , localCount :: !Int
                                 , delegates  :: !DelegateOutput }

type CodegenOutput = DList Instruction

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
cil SNothing      = cgThrowException "SNothing"

-- Special constructors: True, False
cil (SCon _ 0 n []) | n == boolFalse = tell [ ldc_i4 0, boxBoolean ]
cil (SCon _ 1 n []) | n == boolTrue  = tell [ ldc_i4 1, boxBoolean ]

-- General constructors
cil (SCon Nothing t _ fs) = do
  tell [ ldc t
       , ldc $ length fs
       , newarr Cil.Object ]
  mapM_ storeElement (zip [0..] fs)
  tell [ newobj "" recordTypeName [Int32, array] ]
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
    \thenLabel -> do loadRecordTag
                     tell [ ldc tag
                          , beq thenLabel ]

-- In some situations idris gives us a SCase with two default clauses
cil (SCase Shared v [t@SConstCase{}, e@SDefaultCase{}, SDefaultCase{}]) = cil (SCase Shared v [t, e])

cil (SCase Shared v [SConstCase c thenAlt, SDefaultCase elseAlt]) =
  cgIfThenElse v thenAlt elseAlt $ \thenLabel ->
    cgBranchEq c thenLabel

cil (SCase Shared v [c@SConCase{}]) = cgSConCase v c

cil (SCase Shared v alts@(SConstCase (Ch _) _ : _)) = do
  val <- gensym "val"
  load v
  tell [ localsInit [Local Char val]
       , unbox_any Char
       , stlocN val ]
  labels <- uniqueLabelsFor alts
  endLabel <- gensym "END"
  mapM_ (cgAlt endLabel val) (zip labels alts)
  tell [ label endLabel ]
  where
    cgAlt :: String -> String -> (String, SAlt) -> CilCodegen ()
    cgAlt end val (l, SConstCase (Ch t) e) = do
      tell [ ldc $ ord t
           , ldlocN val
           , ceq
           , brfalse l ]
      cil e
      tell [ br end
           , label l ]
    cgAlt end _ (l, SDefaultCase e) = do
      cil e
      tell [ br end ]
    cgAlt _ _ (_, c) = unsupported "char case" c


cil e@(SCase Shared v alts) = let (cases, defaultCase) = partition caseType alts
                              in case defaultCase of
                                   [] -> cgCase v (sorted cases <> [SDefaultCase SNothing])
                                   _  -> cgCase v (sorted cases <> defaultCase)
   where sorted = sortBy (compare `on` tag)
         tag (SConCase _ t _ _ _) = t
         tag (SConstCase (I t) _) = t
         tag c                    = unsupportedCase c
         caseType SDefaultCase{}  = False
         caseType _               = True
         unsupportedCase c        = error $ show c <> " in\n" <> show e

cil (SChkCase _ [SDefaultCase e]) = cil e
cil (SChkCase v alts) = cgCase v alts

cil (SApp isTailCall n args) = do
  forM_ args load
  if isTailCall
    then tell [ tailcall app, ret, ldnull ]
    else tell [ app ]
  where app = call [] Cil.Object "" moduleName (cilName n) (const Cil.Object <$> args)

cil (SForeign retDesc desc args) = emit $ parseDescriptor desc
  where emit :: CILForeign -> CilCodegen ()

        emit (CILDelegate t) =
          cilDelegate t retDesc args

        emit (CILTypeOf t) =
          cilTypeOf t

        emit (CILEnumValueOf t i) =
          tell [ ldc i
               , box t ]

        emit CILConstructor =
          case retType of
            Array _ -> emitNewArray retType
            _       -> emitNewInstance

        emit (CILInstance fn) = do
          let declType : paramTypes = sig
          case declType of
            Array _ -> emitArrayFFI declType fn
            _       -> emitInstanceFFI declType fn paramTypes

        emit ffi = do
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

        ldelemFor :: PrimitiveType -> CodegenOutput
        ldelemFor Int32 = [ ldelem_i4 ]
        ldelemFor ty@ValueType{} = [ ldelema ty
                                   , ldobj ty ]
        ldelemFor ty    = error $ "No ldelem for " ++ show ty

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

        loadTypedArg :: (PrimitiveType, LVar) -> CilCodegen ()
        loadTypedArg (t, loc) = do
          load loc
          castOrUnbox t

        acceptBoxOrPush :: PrimitiveType -> CilCodegen ()
        acceptBoxOrPush Void              = tell [ loadNothing ]
        acceptBoxOrPush t | isValueType t = tell [ box t ]
        acceptBoxOrPush _                 = return ()
        sig                               = foreignType . fst <$> args
        retType                           = foreignType retDesc

cil e = unsupported "expression" e

-- Delegates are emitted as instance functions of the general Record data type
-- so we can avoid the overhead of an additional closure object at runtime
cilDelegate :: PrimitiveType -> FDesc -> [(FDesc, LVar)] -> CilCodegen ()
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

delegateMethodFor :: ForeignFunctionType -> CilCodegen String
delegateMethodFor fft = do
  st@(CodegenState _ _ delegates) <- get
  case M.lookup fft delegates of
    Just (Method _ _ fn _ _) ->
      return fn
    _ -> do
      let fn = "delegate" <> show (M.size delegates)
      let ForeignFunctionType{..} = fft
      let invocation = ldarg 0 : (zip [1..] parameterTypes >>= (<> [apply0]) . loadArg)
      let f = delegateFunction [MaAssembly] returnType fn parameterTypes returnTypeIO invocation
      put $ st { delegates = M.insert fft f delegates }
      return fn
  where apply0 = call [] Cil.Object "" moduleName "APPLY0" [Cil.Object, Cil.Object]

cilTypeOf :: PrimitiveType -> CilCodegen ()
cilTypeOf t = tell [ ldtoken t
                   , call [] runtimeType "mscorlib" "System.Type" "GetTypeFromHandle" [runtimeTypeHandle] ]

delegateFunction :: [MethAttr] -> PrimitiveType -> MethodName -> [PrimitiveType] -> Bool -> [Instruction] -> MethodDef
delegateFunction attrs retType fn paramTypes io invocation = Method attrs retType fn parameters body
  where parameters = zipWith param [(0 :: Int)..] paramTypes
        param i t  = Param Nothing t ("p" <> show i)
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


-- Exported data types are encoded as structs with a single `ptr` field
loadArg :: (Int, PrimitiveType) -> [Instruction]
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
isValueType Double64 = True
isValueType Float32 = True
isValueType Int32   = True
isValueType Bool    = True
isValueType Char    = True
isValueType _       = False

loadRecordTag :: CilCodegen ()
loadRecordTag = tell [ castclass recordTypeRef
                     , ldfld Int32 "" recordTypeName "tag" ]

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
cgConst (Fl d)  = tell [ ldc_r8 d
                       , boxDouble64 ]
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

cgCase :: LVar -> [SAlt] -> CilCodegen ()
cgCase v alts@(SConstCase (I _) _ : _) = cgSwitchCase v alts loadTag altTag
  where loadTag = tell [ unbox_any Int32 ]
        altTag (SConstCase (I t) _) = t
        altTag alt = error $ "expecting (SConstCase (I t)) got: " <> show alt

cgCase v alts = cgSwitchCase v consecutiveAlts loadTag altTag
  where consecutiveAlts = let (caseAlts, defaultAlts) = span isSConCase alts
                          in fillInTheGaps altTag unreachableAlt caseAlts ++ defaultAlts
        unreachableAlt tag = SConCase 0 tag unreachableName [] SNothing
        loadTag = loadRecordTag
        altTag (SConCase _ t _ _ _) = t
        altTag alt = error $ "expecting SConCase got: " <> show alt

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

uniqueLabelsFor :: [a] -> CilCodegen [String]
uniqueLabelsFor alts = do
  uniqueLabelPrefix <- gensym "L"
  pure $ (uniqueLabelPrefix <>) . show <$> [0..(length alts - 1)]

cgSwitchCase :: LVar -> [SAlt] -> CilCodegen () -> (SAlt -> Int) -> CilCodegen ()
cgSwitchCase val alts loadTag altTag | canBuildJumpTable alts = do
  labels <- uniqueLabelsFor alts
  endLabel <- gensym "END"
  load val
  loadTag
  tell [ ldc baseTag
       , sub
       , switch labels
       , br (last labels) ]

  mapM_ (cgAlt endLabel val) (zip labels alts)
  tell [ label endLabel ]
  where canBuildJumpTable (a:as) = canBuildJumpTable' (altTag a) as
        canBuildJumpTable _      = False
        canBuildJumpTable' _ [SDefaultCase _]     = True
        canBuildJumpTable' t (a:as) | t' == t + 1 = canBuildJumpTable' t' as where t' = altTag a
        canBuildJumpTable' _ _                    = False
        baseTag = altTag (head alts)
cgSwitchCase _ alts _ _ = unsupported "switch case alternatives" (descAlt <$> alts)

descAlt :: SAlt -> String
descAlt (SConCase _ t _ _ _) = "SConCase " <> show t
descAlt (SConstCase t _) = "SConstCase " <> show t
descAlt (SDefaultCase _) = "SDefaultCase"

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
    tell [ castclass recordTypeRef
         , ldfld array "" recordTypeName "fields" ]
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
  tell [ call [] String "mscorlib" "System.String" "Concat" (const String <$> args) ]

cgOp LStrCons [h, t] = do
  loadAs Char h
  tell [ call [] String "mscorlib" "System.Char" "ToString" [Char] ]
  loadString t
  tell [ call [] String "mscorlib" "System.String" "Concat" [String, String] ]

cgOp LStrSubstr [index, count, s] = do
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

cgOp LStrEq args = do
  forM_ args loadString
  tell [ call [] Bool "mscorlib" "System.String" "op_Equality" (const String <$> args)
       , boxInt32 ]

cgOp LStrLt args = do
  forM_ args loadString
  tell [ call [CcInstance] Int32 "mscorlib" "System.String" "CompareTo" [String]
       , ldc_i4 0
       , clt
       , boxInt32 ]

cgOp LStrHead [v] = do
  loadString v
  tell [ ldc_i4 0
       , call [CcInstance] Char "mscorlib" "System.String" "get_Chars" [Int32]
       , boxChar ]

cgOp LStrTail [v] = do
  loadString v
  tell [ ldc_i4 1
       , call [CcInstance] String "mscorlib" "System.String" "Substring" [Int32] ]

cgOp LStrIndex [s, i] = do
  loadString s
  loadAs Int32 i
  tell [ callvirt Char "mscorlib" "System.String" "get_Chars" [Int32]
       , boxChar ]

cgOp (LStrInt ITNative) [s] = cgTryParse Int32 s

cgOp (LStrInt i) [_] = unsupported "LStrInt" i

cgOp (LChInt ITNative) [c] = do
  load c
  tell [ unbox_any Char
       , boxInt32 ]

cgOp (LSExt ITNative ITBig) [i]  = load i
cgOp (LZExt ITNative ITBig) [i]  = load i
cgOp (LPlus (ATInt _))      args = cgInt32Op add args
cgOp (LMinus (ATInt _))     args = cgInt32Op sub args
cgOp (LTimes (ATInt _))     args = cgInt32Op mul args
cgOp (LEq (ATInt ITChar))   args = cgPrimitiveOp Char Int32 ceq args
cgOp (LEq (ATInt _))        args = cgInt32Op ceq args
cgOp (LSLt (ATInt ITChar))  args = cgPrimitiveOp Char Int32 clt args
cgOp (LSLt (ATInt _))       args = cgInt32Op clt args
cgOp (LIntStr _)            [i]  = cgPrimitiveToString i
cgOp (LIntFloat _)          [i]  = cgPrimitiveCast Int32 Double64 conv_r8 i
cgOp (LTimes ATFloat)       args = cgFloatOp mul args
cgOp (LSDiv ATFloat)        args = cgFloatOp Cil.div args
cgOp (LPlus ATFloat)        args = cgFloatOp add args
cgOp (LMinus ATFloat)       args = cgFloatOp sub args
cgOp LFloatStr              [f]  = cgPrimitiveToString f
cgOp LStrFloat              [s]  = cgTryParse Double64 s

cgOp (LExternal name) []
  | name == sUN "prim__null" = tell [ ldnull ]

cgOp (LExternal (sn -> "prim__singleFromDouble")) [x]  = cgPrimitiveCast Double64 Float32 conv_r4 x
cgOp (LExternal (sn -> "prim__singleFromInteger")) [x] = cgPrimitiveCast Int32 Float32 conv_r4 x
cgOp (LExternal (sn -> "prim__singleFromInt")) [x]     = cgPrimitiveCast Int32 Float32 conv_r4 x

cgOp (LExternal (sn -> "prim__singleAdd")) args = cgSingleOp add args
cgOp (LExternal (sn -> "prim__singleSub")) args = cgSingleOp sub args
cgOp (LExternal (sn -> "prim__singleMul")) args = cgSingleOp mul args
cgOp (LExternal (sn -> "prim__singleDiv")) args = cgSingleOp Cil.div args

cgOp (LExternal (sn -> "prim__singleCompare")) [x, y] = do
  x' <- storeTemp Float32 x
  tell [ ldlocaN x' ]
  loadAs Float32 y
  tell [ call [CcInstance] Int32 "" "float32" "CompareTo" [Float32]
       , boxInt32 ]

cgOp (LExternal (sn -> "prim__singleMax")) [x, y] = cgSingleMathOp "Max" x y
cgOp (LExternal (sn -> "prim__singleMin")) [x, y] = cgSingleMathOp "Min" x y
cgOp (LExternal (sn -> "prim__singleNeg")) [x] = do
  loadAs Float32 x
  tell [ neg
       , boxFloat32 ]

cgOp (LExternal (sn -> "prim__singleAbs")) [x] = do
  loadAs Float32 x
  tell [ call [] Float32 "mscorlib" "System.Math" "Abs" [Float32]
       , boxFloat32 ]

cgOp (LExternal (sn -> "prim__singleShow")) [x] = cgPrimitiveToString x

cgOp o _ = unsupportedOp o

cgTryParse :: PrimitiveType -> LVar -> CilCodegen ()
cgTryParse ty var = do
  let (asmName, tyName) = assemblyNameAndTypeFrom ty
  loadString var
  val <- gensym "val"
  tell [ localsInit [ Local ty val ] ]
  tell [ ldlocaN val
       , call [] Bool asmName tyName "TryParse" [String, ByRef ty]
       , pop
       , ldlocN val
       , box ty ]

cgSingleMathOp :: String -> LVar -> LVar -> CilCodegen ()
cgSingleMathOp op x y = do
  loadAs Float32 x
  loadAs Float32 y
  tell [ call [] Float32 "mscorlib" "System.Math" op [Float32, Float32]
       , boxFloat32 ]

cgSingleOp = cgPrimitiveOp Float32 Float32

cgPrimitiveCast :: PrimitiveType -> PrimitiveType -> Instruction -> LVar -> CilCodegen ()
cgPrimitiveCast from to inst var = do
  loadAs from var
  tell [ inst
       , box to ]

storeTemp :: PrimitiveType -> LVar -> CilCodegen String
storeTemp localType localVar = do
  tempName <- gensym "temp"
  loadAs localType localVar
  tell [ localsInit [ Local localType tempName ]
       , stlocN tempName ]
  pure tempName

unsupportedOp :: PrimFn -> CilCodegen ()
unsupportedOp = unsupported "operation"

cgPrimitiveToString :: LVar -> CilCodegen ()
cgPrimitiveToString p = load p >> tell [ objectToString ]

objectToString :: Instruction
objectToString = callvirt String "mscorlib" "System.Object" "ToString" []

unsupported :: Show a => String -> a -> CilCodegen ()
unsupported desc v = do
  (CodegenInput decl _) <- ask
  cgThrowException $ "Unsupported " <> desc <> " `" <> show v <> "' in\n" <> show decl

cgThrowException :: String -> CilCodegen ()
cgThrowException message =
  tell [ ldstr message
       , newobj "mscorlib" "System.Exception" [String]
       , throw
       , ldnull ]

gensym :: String -> CilCodegen String
gensym prefix = do
  st@(CodegenState suffix _ _) <- get
  put $ st { nextSuffix = suffix + 1 }
  return $ prefix <> show suffix

cgInt32Op :: Instruction -> [LVar] -> CilCodegen ()
cgInt32Op = cgNumOp Int32

cgFloatOp :: Instruction -> [LVar] -> CilCodegen ()
cgFloatOp = cgNumOp Double64

cgNumOp :: PrimitiveType -> Instruction -> [LVar] -> CilCodegen ()
cgNumOp t = cgPrimitiveOp t t

cgPrimitiveOp :: PrimitiveType -> PrimitiveType -> Instruction -> [LVar] -> CilCodegen ()
cgPrimitiveOp argT resT op args = do
  forM_ args (loadAs argT)
  tell [ op
       , box resT ]

boxDouble64, boxFloat32, boxInt32, boxChar, boxBoolean :: Instruction
boxDouble64 = box Double64
boxFloat32  = box Float32
boxInt32    = box Int32
boxChar     = box Char
boxBoolean  = box Bool

loadAs :: PrimitiveType -> LVar -> CilCodegen ()
loadAs valueType l = load l >> tell [ unbox_any valueType ]

loadString :: LVar -> CilCodegen ()
loadString l = load l >> tell [ castclass String ]

ldc :: (Integral n) => n -> Instruction
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
recordType methods = classDef [CaPrivate] className noExtends noImplements
                              [tag, fields] allMethods []
  where className  = recordTypeName
        tag        = Field [FaPublic, FaInitOnly] Int32 "tag"
        fields     = Field [FaPublic, FaInitOnly] array "fields"
        allMethods = [ctor, toString] <> methods
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

recordTypeRef :: PrimitiveType
recordTypeRef = ReferenceType "" recordTypeName

recordTypeName :: String
recordTypeName = "Record"

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
quoted name = "'" <> (name >>= validChar) <> "'"
  where validChar :: Char -> String
        validChar c = if c == '\''
                         then "\\'"
                         else [c]
