module IRTS.CodegenCilSpec where

import           Control.Applicative ((<$>))
import           Control.Arrow ((>>>))
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
import           System.Exit
import           System.FilePath
import           System.Info (os)
import           System.Process
import           Test.Hspec
import qualified Test.Hspec as H

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
  where takeComment = lines >>> takeWhile (/= "-}") >>> drop 1 >>> unlines

exec :: FilePath -> IO String
exec input = do
  ci <- compileCodegenInfo input output
  codegenCil ci
  peverify output
  mono output
  where output = replaceExtension input "exe"

compileToBytecode :: [FilePath] -> IO ()
compileToBytecode files = traceProcess "idris" (options ++ files)
  where options = ["--typeintype", "--check", "--quiet"]

evalIdris :: Monad m => IState -> StateT IState (ExceptT e m) a -> m (Either e a)
evalIdris istate prog = runExceptT $ evalStateT prog istate

compileCodegenInfo :: String -> String -> IO CodegenInfo
compileCodegenInfo input output = do
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
mono exe = if os == "mingw32"
             then readProcess exe [] ""
             else readProcess "mono" [exe] ""

peverify :: String -> IO ()
peverify exe = traceProcess "peverify" [exe]

traceProcess :: String -> [String] -> IO ()
traceProcess exe args = do
  (code, stdout, stderr) <- readProcessWithExitCode exe args ""
  case code of
    ExitFailure _ -> error $ exe ++ " error: " ++ stdout ++ stderr
    _             -> return ()
