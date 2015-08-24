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
        hasBody (SFun _ _ _ sexp) = someSExp sexp
        someSExp :: SExp -> Bool
        someSExp SNothing              = False
        someSExp (SOp (LExternal _) _) = False
        someSExp _                     = True

moduleName :: String
moduleName = "'λΠ'"

method :: SDecl -> MethodDef
method decl@(SFun name ps _ sexp) = Method attrs retType (cilName name) parameters (toList body)
  where attrs      = [MaStatic, MaAssembly]
        retType    = if isEntryPoint then Cil.Void else Cil.Object
        parameters = map param ps
        param n    = Param Nothing Cil.Object (cilName n)
        body       = let (CodegenState _ lc, cilForSexp) = cilFor decl sexp
                     in if isEntryPoint
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
        locals lc  = fromList [localsInit $ map local [0..(lc - 1)] | lc > 0]
        local i    = Local Cil.Object ("l" ++ show i)
        isEntryPoint = name == entryPointName
        removeLastTailCall :: [MethodDecl] -> [MethodDecl]
        removeLastTailCall [OpCode (Tailcall e), OpCode Ret, OpCode Ldnull] = [OpCode e]
        removeLastTailCall (x:xs) = x:removeLastTailCall xs

-- [Export Main.FFI_CIL ""
--  [ExportFun Main.exportedFunction (FStr "ExportedFunction") (FIO (FCon CIL_Unit)) []
--  ,ExportFun Main.exportedFunction1 (FStr "") (FCon CIL_Str) [FCon CIL_Bool]]]
exportedTypes :: CodegenInfo -> [TypeDef]
exportedTypes ci = map exportedType (exportDecls ci)
  where exportedType :: ExportIFace -> TypeDef
        exportedType (Export (NS (UN (T.unpack -> "FFI_CIL")) ns) _ es) =
            classDef [CaPublic] exportedTypeName noExtends noImplements [] methods []
          where exportedTypeName = T.unpack $ T.intercalate "." ns
                methods = defaultCtorDef : map exportedFunction es

exportedFunction :: Export -> MethodDef
exportedFunction (ExportFun fn@(NS n _) (FStr alias) rt ps) = Method attrs retType exportName parameters body
  where attrs      = [MaPublic, MaStatic]
        retType    = foreignTypeToCilType rt
        exportName = if null alias then cilName n else alias
        parameters = zipWith param [(0 :: Int)..] paramTypes
        param i t  = Param Nothing t ("p" ++ show i)
        paramTypes = map foreignTypeToCilType ps
        body       = loadArgs ++ [invoke, popBoxOrCast, ret]
        loadArgs   = concatMap loadArg (zip [0..] paramTypes)
        loadArg (i, t) = ldarg i : [box t | isValueType t]
        invoke     = call [] Cil.Object "" moduleName (cilName fn) (map (const Cil.Object) ps)
        popBoxOrCast = case retType of
                         Void -> pop
                         t | isValueType t -> box t
                         t -> castclass t

data CodegenState = CodegenState { nextLabel  :: Int
                                 , localCount :: Int }

type CilCodegen a = RWS SDecl (DList MethodDecl) CodegenState a

cilFor :: SDecl -> SExp -> (CodegenState, DList MethodDecl)
cilFor decl sexp = execRWS (cil sexp) decl (CodegenState 0 0)

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
  cgIfThenElse v thenAlt elseAlt $ \thenLabel ->
    tell [ unbox_any Bool
         , brtrue thenLabel ]

cil (SCase Shared v [ SConCase _ tag _ [] thenAlt, SDefaultCase elseAlt ]) =
  cgIfThenElse v thenAlt elseAlt $ \thenLabel -> do
    loadSConTag
    tell [ ldc tag
         , beq thenLabel ]

-- In some situations idris gives us a SCase with two default clauses (as evidenced by idris-game-of-life)
cil (SCase Shared v [t@SConstCase{}, e@SDefaultCase{}, SDefaultCase{}]) = cil (SCase Shared v [t, e])

cil (SCase Shared v [SConstCase c thenAlt, SDefaultCase elseAlt]) =
  cgIfThenElse v thenAlt elseAlt $ \thenLabel ->
    cgBranchEq c thenLabel

cil (SCase Shared v [c@SConCase{}]) = cgSConCase v c

cil e@(SCase Shared v alts) = let (cases, defaultCase) = partition caseType alts
                              in case defaultCase of
                                   [] -> cgCase v (sorted cases ++ [SDefaultCase SNothing])
                                   _  -> cgCase v (sorted cases ++ defaultCase)
   where sorted = sortBy (compare `on` tag)
         tag (SConCase _ t _ _ _) = t
         tag c                    = unsupportedCase c
         caseType SConCase{}      = True
         caseType SDefaultCase{}  = False
         caseType c               = unsupportedCase c
         unsupportedCase c        = error $ show c ++ " in\n" ++ show e

cil (SChkCase _ [SDefaultCase e]) = cil e
cil (SChkCase v alts) = cgCase v alts

cil (SApp isTailCall n args) = do
  forM_ args load
  if isTailCall
    then tell [ tailcall app, ret, ldnull ]
    else tell [ app ]
  where app = call [] Cil.Object "" moduleName (cilName n) (map (const Cil.Object) args)

cil (SForeign returnType (FStr qname) args) =
  case parseAssemblyQualifiedName qname of
    Right (isInstance, assemblyName, typeName, methodName) -> do
      forM_ args loadArg
      if isInstance
         then
          tell [ callvirt cilReturnType assemblyName typeName methodName (map cilType (Prelude.tail args)) ]
         else
          tell [ call [] cilReturnType assemblyName typeName methodName (map cilType args) ]
      maybeBox cilReturnType
    Left  e -> error $ show e
  where loadArg :: (FDesc, LVar) -> CilCodegen ()
        loadArg (t, Loc i) = do tell [ ldarg i ]
                                castOrUnbox (foreignTypeToCilType t)
        cilType (t, _)     = foreignTypeToCilType t
        cilReturnType      = foreignTypeToCilType returnType

cil e = unsupported "expression" e

castOrUnbox :: PrimitiveType -> CilCodegen ()
castOrUnbox t =
  tell [
    if isValueType t
       then unbox_any t
       else castclass t
    ]

maybeBox :: PrimitiveType -> CilCodegen ()
maybeBox t =
  when (isValueType t) $
    tell [ box t ]

isValueType :: PrimitiveType -> Bool
isValueType Int32 = True
isValueType Bool  = True
isValueType _     = False

loadSConTag :: CilCodegen ()
loadSConTag = tell [ castclass (ReferenceType "" "SCon")
                   , ldfld Int32 "" "SCon" "tag" ]

cgIfThenElse :: LVar -> SExp -> SExp -> (String -> CilCodegen ()) -> CilCodegen ()
cgIfThenElse v thenAlt elseAlt cgBranch = do
  thenLabel <- newLabel "THEN"
  endLabel  <- newLabel "END"
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
cgCase v alts | canBuildJumpTable alts = do
  labelPrefix <- newLabel "L"
  let labels = map ((labelPrefix++) . show) [0..(length alts - 1)]
  endLabel <- newLabel "END"
  load v
  loadSConTag
  tell [ ldc baseTag
       , sub
       , switch labels ]
  mapM_ (cgAlt endLabel v) (zip labels alts)
  tell [ label endLabel ]
  where canBuildJumpTable (SConCase _ t _ _ _ : xs) = canBuildJumpTable' t xs
        canBuildJumpTable _                         = False
        canBuildJumpTable' t (SConCase _ t' _ _ _ : xs) | t' == t + 1 = canBuildJumpTable' t' xs
        canBuildJumpTable' _ [SDefaultCase _]                         = True
        canBuildJumpTable' _ _                                        = False
        baseTag = let (SConCase _ t _ _ _) = head alts in t

storeLocal :: Int -> CilCodegen ()
storeLocal i = do
  tell [ stloc i ]
  modify ensureLocal
  where ensureLocal CodegenState{..} = CodegenState nextLabel (max localCount (i + 1))

cgBranchEq :: Const -> String -> CilCodegen ()
cgBranchEq (Ch c) target =
  tell [ unbox_any Char
       , ldc $ ord c
       , beq target ]
cgBranchEq (BI i) target = cgBranchEq (I . fromIntegral $ i) target
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

cgAlt :: Label -> LVar -> (Label, SAlt) -> CilCodegen ()
cgAlt end v (l, alt) = do
  tell [ label l ]
  cg alt
  tell [ br end ]
  where cg c@(SConCase{})   = cgSConCase v c
        cg (SDefaultCase e) = cil e
        cg e                = unsupported "case" e

cgOp :: PrimFn -> [LVar] -> CilCodegen ()
cgOp LWriteStr [_, s] = do
  load s
  tell [ castclass String
       , call [] Void "mscorlib" "System.Console" "Write" [String]
       , loadNothing ]

cgOp LStrConcat args = do
  forM_ args loadString
  tell [ call [] String "mscorlib" "System.String" "Concat" (map (const String) args) ]

cgOp LStrCons [h, t] = do
  loadAs Char h
  tell [ call [] String "mscorlib" "System.Char" "ToString" [Char] ]
  loadString t
  tell [ call [] String "mscorlib" "System.String" "Concat" [String, String] ]

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
cgOp (LPlus (ATInt _))      args = intOp add args
cgOp (LMinus (ATInt _))     args = intOp sub args
cgOp (LEq (ATInt _))        args = intOp ceq args
cgOp (LSLt (ATInt _))       args = intOp clt args
cgOp (LIntStr _)            [i]  = do
  load i
  tell [ objectToString ]
cgOp o _ = unsupported "operation" o

objectToString :: MethodDecl
objectToString = callvirt String "mscorlib" "System.Object" "ToString" []

unsupported :: Show a => String -> a -> CilCodegen ()
unsupported desc v = do
  decl <- ask
  throwException $ "Unsupported " ++ desc ++ " `" ++ show v ++ "' in\n" ++ show decl

throwException :: String -> CilCodegen ()
throwException message =
  tell [ ldstr message
       , newobj "mscorlib" "System.Exception" [String]
       , throw
       , ldnull ]

newLabel :: String -> CilCodegen String
newLabel prefix = do
  (CodegenState suffix locals) <- get
  put (CodegenState (suffix + 1) locals)
  return $ prefix ++ show suffix

intOp :: MethodDecl -> [LVar] -> CilCodegen ()
intOp op args = do
  forM_ args loadInt32
  tell [ op
       , boxInt32 ]

boxInt32, boxChar, boxBoolean :: MethodDecl
boxInt32 = box Int32
boxChar = box Char
boxBoolean = box Bool

loadInt32 :: LVar -> CilCodegen ()
loadInt32 = loadAs Int32

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

localIndex :: Offset -> CilCodegen Offset
localIndex i = do
  (SFun _ ps _ _) <- ask
  return $ i - length ps

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
        nothing   = Field [FaStatic, FaPublic] Cil.Object "Default"
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
        sconTag    = Field [FaPublic] Int32 "tag"
        sconFields = Field [FaPublic] array "fields"
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

array :: PrimitiveType
array = Array Cil.Object

boolFalse, boolTrue :: Name
boolFalse = NS (UN "False") ["Bool", "Prelude"]
boolTrue  = NS (UN "True")  ["Bool", "Prelude"]

quoted :: String -> String
quoted n = "'" ++ concatMap validChar n ++ "'"
  where validChar :: Char -> String
        validChar c = if c == '\''
                         then "\\'"
                         else [c]
