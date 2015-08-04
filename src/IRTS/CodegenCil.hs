{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module IRTS.CodegenCil where

import           Control.Monad.RWS.Strict hiding (local)
import           Data.DList (DList, fromList, toList, append)
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

codegenCil :: CodeGenerator
codegenCil ci = do writeFile cilFile $ pr (assemblyFor ci) ""
                   when (outputExtension /= ".il") $
                     ilasm cilFile output
  where cilFile = replaceExtension output "il"
        output  = outputFile ci
        outputExtension = takeExtension output

ilasm :: String -> String -> IO ()
ilasm input output = readProcess "ilasm" [input, "/output:" ++ output] "" >>= putStr

assemblyFor :: CodegenInfo -> Assembly
assemblyFor ci = Assembly [mscorlibRef] asmName [moduleFor ci, sconType, consType]
  where asmName  = quoted $ takeBaseName (outputFile ci)

quoted :: String -> String
quoted n = "'" ++ map validChar n ++ "'"
  where validChar c = if c == '\''
                         then '`'
                         else c

moduleFor :: CodegenInfo -> TypeDef
moduleFor ci = classDef [CaPrivate] "M" noExtends noImplements [] methods []
  where methods       = map method declsWithBody
        declsWithBody = filter hasBody decls
        decls         = map snd $ simpleDecls ci
        hasBody (SFun _ _ _ sexp) = someSExp sexp

method :: SDecl -> MethodDef
method decl@(SFun name ps lc sexp) = Method attrs retType (defName name) parameters body
  where attrs      = [MaStatic, MaAssembly]
        retType    = Cil.Object
        parameters = map param ps
        param n    = Param Nothing Cil.Object (cilName n)
        body       = toList $
                      --append (consoleWriteLine (show name)) $ -- trace execution
                       if isEntryPoint
                         then append (append [entryPoint] cilForSexp)
                                     [ret]
                         else append cilForSexp
                                     [ret]
        cilForSexp = append locals (cilFor decl sexp)
        locals     = fromList [localsInit $ map local [0..(lc - 1)] | lc > 0]
        local i    = Local Cil.Object ("l" ++ show i)
        isEntryPoint = name == entryPointName

type CilCodegen a = RWS SDecl (DList MethodDecl) Int a

cilFor :: SDecl -> SExp -> DList MethodDecl
cilFor decl sexp = snd $ evalRWS (cil sexp) decl 0

cil :: SExp -> CilCodegen ()
cil (SLet (Loc _) SNothing e) = cil e
cil (SLet (Loc i) v e) = do
  cil v
  li <- localIndex i
  tell [ stloc li ]
  cil e

cil (SV v) = load v
cil (SConst c) = cgConst c
cil SNothing = tell [ldnull]
cil (SOp op args) = cgOp op args

-- Special constructors: True, False, List.Nil, List.::
cil (SCon _ 0 n []) | n == boolFalse = tell [ ldc_i4 0, box systemBoolean ]
cil (SCon _ 1 n []) | n == boolTrue  = tell [ ldc_i4 1, box systemBoolean ]
cil (SCon _ 0 n []) | n == listNil   = tell [ loadNil ]
cil (SCon _ 1 n [x, xs]) | n == listCons = do load x
                                              load xs
                                              tell [ castclass consTypeRef
                                                   , newobj "" "Cons" [Cil.Object, consTypeRef] ]
-- General constructors
cil (SCon _ t _ fs) = do
  tell [ ldc t
       , ldc $ length fs
       , newarr Cil.Object
       ]
  mapM_ storeElement (zip [0..] fs)
  tell [newobj "" "SCon" [Int32, array]]
  where storeElement (i, f) = do
          tell [dup, ldc_i4 i]
          load f
          tell [stelem_ref]

-- ifThenElse
cil (SCase Shared v [ SConCase _ 0 nFalse [] elseAlt
                    , SConCase _ 1 nTrue  [] thenAlt ]) | nFalse == boolFalse && nTrue == boolTrue = do
  thenLabel <- newLabel "THEN"
  endLabel  <- newLabel "END"
  load v
  tell [ unbox_any systemBoolean
       , brtrue thenLabel ]
  cil elseAlt
  tell [ br endLabel
       , label thenLabel ]
  cil thenAlt
  tell [ label endLabel ]

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
       , ldfld Cil.Object "" "Cons" "car"
       , bind x
       , ldfld consTypeRef "" "Cons" "cdr"
       , bind xs
       ]
  cil consAlt
  tell [ br endLabel
       , label nilLabel ]
  cil nilAlt
  tell [ label endLabel ]
  where bind (MN i _) = stloc i

-- special handling for the common case of branching on 0
cil (SCase Shared v [SConstCase (BI 0) thenAlt, SDefaultCase elseAlt]) = do
  elseLabel <- newLabel "ELSE"
  endLabel  <- newLabel "END"
  loadInteger v
  tell [ brtrue elseLabel ]
  cil thenAlt
  tell [ br endLabel
       , label elseLabel ]
  cil elseAlt
  tell [ label endLabel ]

cil e@(SCase Shared _ _) = tell [ comment $ "NOT IMPLEMENTED: " ++ show e
                                , ldnull ]

cil (SChkCase _ [SDefaultCase e]) = cil e
cil (SChkCase v alts) | canBuildJumpTable alts = do
  load v
  tell [ castclass (ReferenceType "" "SCon")
       , ldfld Int32 "" "SCon" "tag"
       , ldc baseTag
       , sub
       , switch labels
       ]
  mapM_ (cgAlt v) (zip labels alts)
  tell [ label "END" ]
  where canBuildJumpTable (SConCase _ t _ _ _ : xs) = canBuildJumpTable' t xs
        canBuildJumpTable _                         = False
        canBuildJumpTable' t (SConCase _ t' _ _ _ : xs) | t' == t + 1 = canBuildJumpTable' t' xs
        canBuildJumpTable' _ [SDefaultCase _]                         = True
        canBuildJumpTable' _ _                                        = False
        baseTag = let (SConCase _ t _ _ _) = head alts in t
        labels = map (("L"++) . show) [0..(length alts - 1)]

cil (SApp isTailCall n args) = do
  mapM_ load args
  if isTailCall
    then tell [ tailcall app, ret, ldnull ]
    else tell [ app ]
  where app = call [] Cil.Object "" "M" (defName n) (map (const Cil.Object) args)

cil e = do
  decl <- get
  error $ "Unsupported expression `" ++ show e ++ "' in\n" ++ show decl

systemBoolean :: PrimitiveType
systemBoolean = ValueType "mscorlib" "System.Boolean"

ldc :: (Integral n) => n -> MethodDecl
ldc = ldc_i4 . fromIntegral

cgConst :: Const -> CilCodegen ()
cgConst (Str s) = tell [ ldstr s ]
cgConst (BI i)  = tell [ ldc i
                       , boxInteger ]
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

cgAlt :: LVar -> (Label, SAlt) -> CilCodegen ()
cgAlt v (l, SConCase _ _ _ fs sexp) = do
  tell [ label l ]
  unless (null fs) $ do
    load v
    tell [ castclass sconTypeRef
         , ldfld array "" "SCon" "fields"
         ]
    mapM_ loadElement (zip [0..] fs)
    tell [ pop ]
  cil sexp
  tell [ br "END" ]
  where loadElement :: (Int, Name) -> CilCodegen ()
        loadElement (e, MN i _) = tell [ dup
                                       , ldc e
                                       , ldelem_ref
                                       , stloc i
                                       ]
cgAlt _ (l, e) = tell [ label l
                      , comment $ "NOT IMPLEMENTED: " ++ show e
                      , ldnull
                      , br "END"
                      ]

cgOp :: PrimFn -> [LVar] -> CilCodegen ()
cgOp LWriteStr [_, s] = do
  load s
  tell [ castclass String
       , call [] Void "mscorlib" "System.Console" "Write" [String]
       , ldnull
       ]

cgOp LStrConcat args = do
  forM_ args loadString
  tell [ call [] String "mscorlib" "System.String" "Concat" (map (const String) args) ]

cgOp (LPlus _)  args = integerOp add args
cgOp (LMinus _) args = integerOp sub args

cgOp (LIntStr _) [i] = do
  load i
  tell [ callvirt String "mscorlib" "System.Object" "ToString" [] ]

cgOp o _ = error $ "Unsupported operation: " ++ show o

newLabel :: String -> CilCodegen String
newLabel prefix = do
  suffix <- get
  modify (+1)
  return $ prefix ++ show suffix

integerOp :: MethodDecl -> [LVar] -> CilCodegen ()
integerOp op args = do
  forM_ args loadInteger
  tell [ op
       , boxInteger ]

boxInteger :: MethodDecl
boxInteger = box (ValueType "mscorlib" "System.Int32")

loadInteger :: LVar -> CilCodegen ()
loadInteger l = do
  load l
  tell [ unbox_any integerType ]

loadString :: LVar -> CilCodegen ()
loadString l = do
  load l
  tell [ castclass String ]

loadNil :: MethodDecl
loadNil = ldsfld consTypeRef "" "Cons" "Nil"

load :: LVar -> CilCodegen ()
load (Loc i) = do
  li <- localIndex i
  tell [
    if li < 0
      then ldarg i
      else ldloc li
    ]

localIndex :: Offset -> CilCodegen Offset
localIndex i = do
  (SFun _ ps _ _) <- ask
  return $ i - length ps

entryPointName :: Name
--entryPointName = NS (UN "main") ["Main"]
entryPointName = MN 0 "runMain"

someSExp :: SExp -> Bool
someSExp SNothing              = False
someSExp (SOp (LExternal _) _) = False
someSExp _                     = True

-- | Turns an arbitrary name into a valid CIL name.
-- |
-- | http://www.unicode.org/reports/tr15/tr15-18.html#Programming Language Identifiers
-- | <identifier> ::= <identifier_start> ( <identifier_start> | <identifier_extend> )*
-- | <identifier_start> ::= [{Lu}{Ll}{Lt}{Lm}{Lo}{Nl}]
-- | <identifier_extend> ::= [{Mn}{Mc}{Nd}{Pc}{Cf}]
-- |
-- | That is, the first character of an identifier can be an uppercase
-- | letter, lowercase letter, titlecase letter, modifier letter, other
-- | letter, or letter number. The subsequent characters of an
-- | identifier can be any of those, plus non-spacing marks, spacing
-- | combining marks, decimal numbers, connector punctuations, and
-- | formatting codes (such as right-left-mark). Normally the formatting
-- | codes should be filtered out before storing or comparing
-- | identifiers.
cilName :: Name -> String
cilName (UN t)   = quoted $ T.unpack t
cilName (MN i t) = quoted $ T.unpack t ++ show i
cilName (SN sn)  = quoted $ show sn
cilName e = error $ "Unsupported name `" ++ show e ++ "'"

defName :: Name -> MethodName
defName (NS n _) = cilName n
defName n = cilName n

consType :: TypeDef
consType = classDef [CaPrivate] className noExtends noImplements
                    [car, cdr, nil] [ctor, cctor] []
  where className = "Cons"
        nil       = Field [FaStatic, FaPublic] consTypeRef "Nil"
        cctor     = Constructor [MaStatic] Void []
                      [ ldnull
                      , ldnull
                      , newobj "" className [Cil.Object, consTypeRef]
                      , stsfld consTypeRef "" className "Nil"
                      , ret
                      ]
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
                      , ret
                      ]

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
                     , ret
                     ]

consTypeRef, sconTypeRef :: PrimitiveType
consTypeRef = ReferenceType "" "Cons"
sconTypeRef = ReferenceType "" "SCon"

array :: PrimitiveType
array = Array Cil.Object

integerType :: PrimitiveType
integerType = ValueType "mscorlib" "System.Int32"

consoleWriteLine :: String -> DList MethodDecl
consoleWriteLine s = [ ldstr s
                     , call [] Void "mscorlib" "System.Console" "WriteLine" [String]
                     ]

boolFalse, boolTrue, listNil, listCons :: Name

boolFalse = NS (UN "False") ["Bool", "Prelude"]
boolTrue  = NS (UN "True") ["Bool", "Prelude"]

listNil  = NS (UN "Nil") ["List", "Prelude"]
listCons = NS (UN "::")  ["List", "Prelude"]
