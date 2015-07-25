{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module IRTS.CodegenCil where

import           Control.Monad (unless)
import           Control.Monad.State (State, get, evalState)
import           Control.Monad.Trans.Writer.Strict (WriterT, tell, execWriterT)
import           Data.Char
import           Data.DList (DList, fromList, toList, append)
import qualified Data.Text as T
import           IRTS.CodegenCommon
import           IRTS.Lang
import           IRTS.Simplified
import           Idris.Core.TT
import           Language.Cil
import qualified Language.Cil as Cil
import           System.FilePath (takeBaseName)

codegenCil :: CodeGenerator
codegenCil ci = writeFile (outputFile ci) $ pr (assemblyFor ci) ""

assemblyFor :: CodegenInfo -> Assembly
assemblyFor ci = Assembly [mscorlibRef] asmName [moduleFor ci, sconType]
  where asmName  = quoted $ takeBaseName (outputFile ci)
        quoted n = "'" ++ n ++ "'"

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

type CilCodegen a = WriterT (DList MethodDecl) (State SDecl) a

cilFor :: SDecl -> SExp -> DList MethodDecl
cilFor decl sexp = evalState (execWriterT $ cil sexp) decl

cil :: SExp -> CilCodegen ()
cil (SLet (Loc _) SNothing e) = cil e
cil (SLet (Loc i) v e) = do
  cil v
  li <- localIndex i
  tell [stloc li]
  cil e

cil (SV v) = load v
cil (SConst (Str s)) = tell [ldstr s]
cil SNothing = tell [ldnull]
cil (SOp op args) = cgOp op args

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
  tell [label "END"]
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
    then tell [tailcall app]
    else tell [app]
  where app = call [] Cil.Object "" "M" (defName n) (map (const Cil.Object) args)

cil e = do
  decl <- get
  error $ "Unsupported expression `" ++ show e ++ "' in\n" ++ show decl

ldc :: (Integral n) => n -> MethodDecl
ldc = ldc_i4 . fromIntegral

cgAlt :: LVar -> (Label, SAlt) -> CilCodegen ()
cgAlt v (l, SConCase _ _ _ fs sexp) = do
  tell [label l]
  unless (null fs) $ do
    load v
    tell [ castclass (ReferenceType "" "SCon")
         , ldfld array "" "SCon" "fields"
         ]
    mapM_ loadElement (zip [0..] fs)
    tell [pop]
  cil sexp
  tell [ret]
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
--cgAlt _ (l, e@(SConstCase t exp)) = tell [label l, comment (show e), ldnull, br "END"]
--cgAlt _ (l, e@(SDefaultCase exp)) = tell [label l, comment (show e), ldnull, br "END"]


cgOp :: PrimFn -> [LVar] -> CilCodegen ()
cgOp LWriteStr [_, s] = do
  load s
  tell [ castclass String
       , call [] Void "mscorlib" "System.Console" "Write" [String]
       , ldnull
       ]
cgOp o _ = error $ "Unsupported operation: " ++ show o

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
  (SFun _ ps _ _) <- get
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
cilName (UN t)   = T.unpack t
cilName (MN i t) = T.unpack t ++ show i
cilName e = error $ "Unsupported name `" ++ show e ++ "'"

defName :: Name -> MethodName
defName (NS n _) = cilName n
defName (SN sn)  = map validChar (show sn)
  where validChar ch =
          if isIdentifierChar ch
             then ch
             else '_'
        isIdentifierChar ch = isAlphaNum ch || (isMark ch && nonSpace ch)
        nonSpace = Prelude.not . isSpace
defName n = cilName n

sconType :: TypeDef
sconType = classDef [CaPrivate] className noExtends noImplements
                    [sconTag, sconFields] [sconCtor] []
  where className  = "SCon"
        sconTag    = Field [FaPublic] Int32 "tag"
        sconFields = Field [FaPublic] array "fields"
        sconCtor   = Constructor [MaPublic] Void [ Param Nothing Int32 "tag"
                                                 , Param Nothing array "fields"]
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

array :: PrimitiveType
array = Array Cil.Object

consoleWriteLine :: String -> DList MethodDecl
consoleWriteLine s = [ ldstr s
                     , call [] Void "mscorlib" "System.Console" "WriteLine" [String]
                     ]
