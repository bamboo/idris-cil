{-# LANGUAGE RecordWildCards, OverloadedStrings, OverloadedLists #-}
module IRTS.CodegenCil where

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

import           IRTS.Cil.UnreachableCodeRemoval

codegenCil :: CodeGenerator
codegenCil ci = do BS.writeFile cilFile $ UTF8.fromString cilText
                   when (outputExtension /= ".il") $
                     ilasm cilFile output
  where cilFile = replaceExtension output "il"
        cilText = pr (assemblyFor ci) ""
        output  = outputFile ci
        outputExtension = takeExtension output

ilasm :: String -> String -> IO ()
ilasm input output = readProcess "ilasm" [input, "/output:" ++ output] "" >>= putStr

assemblyFor :: CodegenInfo -> Assembly
assemblyFor ci = Assembly [mscorlibRef] asmName [moduleFor ci, sconType, consType]
  where asmName  = quoted $ takeBaseName (outputFile ci)

moduleFor :: CodegenInfo -> TypeDef
moduleFor ci = classDef [CaPrivate] moduleName noExtends noImplements [] methods []
  where methods       = removeUnreachable $ map method declsWithBody
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
                              `append`  locals lc
                              `append` cilForSexp
                              `append` [ret]
        locals lc  = fromList [localsInit $ map local [0..(lc - 1)] | lc > 0]
        local i    = Local Cil.Object ("l" ++ show i)
        isEntryPoint = name == entryPointName
        removeLastTailCall :: [MethodDecl] -> [MethodDecl]
        removeLastTailCall [OpCode (Tailcall e), OpCode Ret, OpCode Ldnull] = [OpCode e]
        removeLastTailCall (x:xs) = x:removeLastTailCall xs


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
  li <- localIndex i
  storeLocal li
  cil e

cil (SUpdate (Loc i) v) = do
  cil v
  tell [ dup ]
  li <- localIndex i
  storeLocal li

cil (SV v) = load v
cil (SConst c) = cgConst c
cil SNothing = throwException "SNothing"
cil (SOp op args) = cgOp op args

-- Special constructors: True, False, List.Nil, List.::
cil (SCon _ 0 n []) | n == boolFalse = tell [ ldc_i4 0, boxBoolean ]
cil (SCon _ 1 n []) | n == boolTrue  = tell [ ldc_i4 1, boxBoolean ]
cil (SCon _ 0 n []) | n == listNil   = tell [ loadNil ]
cil (SCon _ 1 n [x, xs]) | n == listCons = do load x
                                              load xs
                                              tell [ castclass consTypeRef
                                                   , newobj "" "Cons" [Cil.Object, consTypeRef] ]
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

cil (SCase Shared v [ SConCase _ c _ [] thenAlt, SDefaultCase elseAlt ]) =
  cgIfThenElse v thenAlt elseAlt $ \thenLabel -> do
    loadSConTag
    tell [ ldc c
         , beq thenLabel ]

cil (SCase Shared v [ SConstCase c thenAlt, SDefaultCase elseAlt ]) =
  cgIfThenElse v thenAlt elseAlt $ \thenLabel ->
    cgBranchEq c thenLabel

-- List case matching
cil (SCase Shared v [ SConCase _ 1 nCons [x, xs] consAlt
                    , SConCase _ 0 nNil  []      nilAlt ]) | nCons == listCons && nNil == listNil = do

  nilLabel <- newLabel "NIL"
  endLabel <- newLabel "END"

  load v
  tell [ loadNil
       , beq nilLabel ]
  load v
  tell [ castclass consTypeRef
       , dup
       , ldfld Cil.Object "" "Cons" "car" ]
  bind x
  tell [ ldfld consTypeRef "" "Cons" "cdr" ]
  bind xs
  cil consAlt
  tell [ br endLabel
       , label nilLabel ]
  cil nilAlt
  tell [ label endLabel ]
  where bind (MN i _) = storeLocal i

cil (SCase Shared v [c@SConCase{}]) = cgSConCase v c

{-
Case Shared (Loc 4) [SConCase 5 0 Data.SortedMap.Empty [{in0}] (SLet (Loc 6) (SCon Nothing 0 Data.SortedMap.Leaf [Loc 2,Loc 3]) (SCon Nothing 1 Data.SortedMap.M [Loc 5,Loc 6]))
                    ,SConCase 5 1 Data.SortedMap.M [{in1},{in2}] (SLet (Loc 7) (SLet (Loc 7) SNothing (SLet (Loc 8) SNothing (SLet (Loc 9) SNothing (SApp False Data.SortedMap.treeInsert [Loc 7,Loc 8,Loc 9,Loc 5,Loc 2,Loc 3,Loc 6])))) (SCase Shared (Loc 7) [SConCase 8 0 Prelude.Either.Left [{in3}] (SCon Nothing 1 Data.SortedMap.M [Loc 5,Loc 8])
            ,SConCase 8 1 Prelude.Either.Right [{in4}] (SCon Nothing 1 Data.SortedMap.M [Loc 5,Loc 8])]))]
-}

cil (SCase Shared v alts) = let (cases, defaultCase) = partition caseType alts
                            in case defaultCase of
                                    []      -> cgCase v (sorted cases ++ [SDefaultCase SNothing])
                                    _       -> cgCase v (sorted cases ++ defaultCase)
   where sorted = sortBy (compare `on` tag)
         tag (SConCase _ t _ _ _) = t
         tag c                    = error $ show c
         caseType SConCase{}      = True
         caseType SDefaultCase{}  = False

cil (SChkCase _ [SDefaultCase e]) = cil e
cil (SChkCase v alts) = cgCase v alts

cil (SApp isTailCall n args) = do
  mapM_ load args
  if isTailCall
    then tell [ tailcall app, ret, ldnull ]
    else tell [ app ]
  where app = call [] Cil.Object "" moduleName (cilName n) (map (const Cil.Object) args)

cil e = unsupported "expression" e

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
cgSConCase v (SConCase _ _ _ fs sexp) = do
  unless (null fs) $ do
    load v
    tell [ castclass sconTypeRef
         , ldfld array "" "SCon" "fields" ]
    mapM_ loadElement (zip [0..] fs)
    tell [ pop ]
  cil sexp
  where loadElement :: (Int, Name) -> CilCodegen ()
        loadElement (e, MN i _) = do
          tell [ dup
               , ldc e
               , ldelem_ref ]
          storeLocal i

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

-- cgOp LStrHead [v] = do
--   loadString v
--   tell [ ldc_i4 0
--        , call [CcInstance] Char "mscorlib" "System.String" "get_Chars" [Int32]
--        , boxChar ]

-- cgOp LStrTail [v] = do
--   loadString v
--   tell [ ldc_i4 1
--        , call [CcInstance] String "mscorlib" "System.String" "Substring" [Int32] ]

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

loadInt32, loadChar :: LVar -> CilCodegen ()
loadInt32 = loadAs Int32
loadChar  = loadAs Char

loadAs :: PrimitiveType -> LVar -> CilCodegen ()
loadAs valueType l = do
  load l
  tell [ unbox_any valueType ]

loadString :: LVar -> CilCodegen ()
loadString l = do
  load l
  tell [ castclass String ]

loadNil :: MethodDecl
loadNil = ldsfld consTypeRef "" "Cons" "Nil"

ldc :: (Integral n) => n -> MethodDecl
ldc = ldc_i4 . fromIntegral

load :: LVar -> CilCodegen ()
load (Loc i) = do
  li <- localIndex i
  tell [
    if li < 0
      then ldarg i
      else ldloc li ]

loadNothing :: MethodDecl
loadNothing = ldsfld Cil.Object "" "Cons" "Nothing"

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

consType :: TypeDef
consType = classDef [CaPrivate] className noExtends noImplements
                    [car, cdr, nil, nothing] [ctor, cctor] []
  where className = "Cons"
        nil       = Field [FaStatic, FaPublic] consTypeRef "Nil"
        nothing   = Field [FaStatic, FaPublic] Cil.Object "Nothing"
        cctor     = Constructor [MaStatic] Void []
                      [ loadNothing
                      , ldnull
                      , newobj "" className [Cil.Object, consTypeRef]
                      , stsfld consTypeRef "" className "Nil"
                      , newobj "mscorlib" "System.Object" []
                      , stsfld Cil.Object "" className "Nothing"
                      , ret ]
        car       = Field [FaPublic] Cil.Object "car"
        cdr       = Field [FaPublic] consTypeRef "cdr"
        ctor      = Constructor [MaPublic] Void [ Param Nothing Cil.Object "car"
                                                , Param Nothing consTypeRef "cdr" ]
                      [ ldarg 0
                      , call [CcInstance] Void "" "object" ".ctor" []
                      , ldarg 0
                      , ldarg 1
                      , stfld Cil.Object "" className "car"
                      , ldarg 0
                      , ldarg 2
                      , stfld consTypeRef "" className "cdr"
                      , ret ]

sconType :: TypeDef
sconType = classDef [CaPrivate] className noExtends noImplements
                    [sconTag, sconFields] [sconCtor] []
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

consTypeRef, sconTypeRef :: PrimitiveType
consTypeRef = ReferenceType "" "Cons"
sconTypeRef = ReferenceType "" "SCon"

systemBoolean, systemChar, systemInt32, array :: PrimitiveType
systemBoolean = ValueType "mscorlib" "System.Boolean"
systemChar    = ValueType "mscorlib" "System.Char"
systemInt32   = ValueType "mscorlib" "System.Int32"
array          = Array Cil.Object

boolFalse, boolTrue, listNil, listCons :: Name
boolFalse = NS (UN "False") ["Bool", "Prelude"]
boolTrue  = NS (UN "True")  ["Bool", "Prelude"]
listNil   = NS (UN "Nil")   ["List", "Prelude"]
listCons  = NS (UN "::")    ["List", "Prelude"]

quoted :: String -> String
quoted n = "'" ++ concatMap validChar n ++ "'"
  where validChar :: Char -> String
        validChar c = if c == '\''
                         then "\\'"
                         else [c]

consoleWriteLine :: String -> DList MethodDecl
consoleWriteLine s = [ ldstr s
                     , call [] Void "mscorlib" "System.Console" "WriteLine" [String] ]
