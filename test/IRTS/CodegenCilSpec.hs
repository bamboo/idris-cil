module IRTS.CodegenCilSpec where

import           Control.Arrow ((>>>))
import           Control.Monad (forM_)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT)
import           Data.Monoid ((<>))

import           IRTS.Cil.Builders
import           IRTS.CodegenCil
import           Idris.AbsSyntax
import           Idris.ElabDecls
import           Language.Cil as Cil

import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Info (os)
import           System.Process

import           Test.Hspec (Spec, parallel, describe, it)
import qualified Test.Hspec as H
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec = describe "idris-cil" $ do
  files <- H.runIO testCaseFiles
  H.runIO $ compileToBytecode files
  parallel $ forM_ files testCaseForFile

testCaseFiles :: IO [FilePath]
testCaseFiles = do
  cd <- getCurrentDirectory
  listFilesWithExtension ".idr" (cd </> "test/cases")

testCaseForFile :: FilePath -> Spec
testCaseForFile f =
  it ("can compile `" <> takeBaseName f <> "'") $ do
    output   <- exec f
    expected <- firstCommentIn f
    output `shouldBe` expected

listFilesWithExtension :: String -> FilePath -> IO [FilePath]
listFilesWithExtension ext dir = do
  files <- getDirectoryContents dir
  return $ (dir </>) <$> filter ((== ext) . takeExtension) files

firstCommentIn :: FilePath -> IO String
firstCommentIn f = takeComment <$> readFile f
  where takeComment = lines >>> takeWhile (/= "-}") >>> drop 1 >>> unlines

exec :: FilePath -> IO String
exec input = do
  compileCodegenInfo input output >>= codegenCilTrans introduceFixtureType
  --peverify output
  mono output
  where output = replaceExtension input "exe"

introduceFixtureType :: Assembly -> Assembly
introduceFixtureType (Assembly refs name types) =
  Assembly refs name (fixtureType : types)

fixtureType :: TypeDef
fixtureType = publicSealedClass className noExtends noImplements allFields allMethods []
  where className  = "Fixture"
        allFields  = []
        allMethods = [defaultCtorDef, passStringByRef]
        passStringByRef =
          Method [MaStatic, MaPublic] Cil.Void "PassStringByRef" [Param (Just PaOut) (ByRef String) "s"]
                       [ ldarg 0
                       , ldstr "The String"
                       , stind_ref
                       , ret ]

compileToBytecode :: [FilePath] -> IO ()
compileToBytecode files = traceProcess "idris" (options <> files)
  where options = ["--typeintype", "--check", "--quiet", "-p", "cil", "-p", "contrib", "-p", "effects"]

evalIdris :: Monad m => IState -> StateT IState (ExceptT e m) a -> m (Either e a)
evalIdris istate prog = runExceptT $ evalStateT prog istate

compileCodegenInfo :: String -> String -> IO CilCodegenInfo
compileCodegenInfo input output = do
  maybeCI <- evalIdris idrisInit $ compileCilCodegenInfo [bytecodeFile] output
  case maybeCI of
    Left  e  -> error $ show e
    Right ci -> return ci
  where bytecodeFile = replaceExtension input "ibc"

mono :: String -> IO String
mono exe = if os == "mingw32"
             then readProcess exe [] ""
             else readProcess "dotnet" [exe] ""

peverify :: String -> IO ()
peverify exe = traceProcess "peverify" [exe]

traceProcess :: String -> [String] -> IO ()
traceProcess exe args = do
  (code, stdout, stderr) <- readProcessWithExitCode exe args ""
  case code of
    ExitFailure _ -> error $ exe <> " error: " <> stdout <> stderr
    _             -> return ()
