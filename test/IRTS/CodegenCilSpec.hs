module IRTS.CodegenCilSpec where

import           Control.Applicative ((<$>))
import           Control.Monad (forM_)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT)
import           IRTS.CodegenCil
import           IRTS.CodegenCommon
import           IRTS.Compiler
import           Idris.AbsSyntax
import           Idris.ElabDecls
import           Idris.REPL
import           System.Directory
import           System.FilePath
import           System.Process
import           Test.Hspec
import qualified Test.Hspec as H

spec :: Spec
spec = describe "idris-cil" $ do
  files <- H.runIO testCaseFiles
  forM_ files testCaseForFile

testCaseFiles :: IO [FilePath]
testCaseFiles = do
  cd <- getCurrentDirectory
  listFilesWithExtension ".idr" (cd </> "test/cases")

testCaseForFile :: FilePath -> Spec
testCaseForFile f =
  it ("can compile `" ++ takeBaseName f ++ "'") $ do
    output   <- exec f
    expected <- firstCommentIn f
    output `shouldBe` expected

listFilesWithExtension :: String -> FilePath -> IO [FilePath]
listFilesWithExtension ext dir = do
  files <- getDirectoryContents dir
  return $ map (dir </>) $ filter ((== ext) . takeExtension) files

firstCommentIn :: FilePath -> IO String
firstCommentIn f = takeComment <$> readFile f
  where takeComment = unlines . takeWhile (/= "-}") . drop 1 . lines

exec :: FilePath -> IO String
exec input = do
  ci <- compileCodegenInfo input output
  codegenCil ci
  mono output
  where output = replaceExtension input "exe"

evalIdris :: Monad m => IState -> StateT IState (ExceptT e m) a -> m (Either e a)
evalIdris istate prog = runExceptT $ evalStateT prog istate

compileCodegenInfo :: String -> String -> IO CodegenInfo
compileCodegenInfo input output = do
  runMain $ idrisMain [Filename input, UseCodegen Bytecode, NoCoverage, NoREPL, NoBanner, Quiet]
  Right ci <- evalIdris idrisInit $ codegenInfoFrom [bytecodeFile] output
  return ci
  where bytecodeFile = replaceExtension input "ibc"

codegenInfoFrom :: [FilePath] -> FilePath -> Idris CodegenInfo
codegenInfoFrom inputs output = do
  elabPrims
  loadInputs inputs Nothing
  mainProg <- elabMain
  compile (Via "cil") output (Just mainProg)

mono :: String -> IO String
mono exe = readProcess "mono" [exe] ""
