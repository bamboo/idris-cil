module IRTS.CodegenCilSpec where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import IRTS.CodegenCil
import IRTS.CodegenCommon
import IRTS.Compiler
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.REPL
import System.FilePath
import Test.Hspec
import System.Process

spec :: Spec
spec =
  describe "idris-cil" $
    it "can compile `Hello, World!`" $ do
      let code     = "module Main\n\
                     \main : IO ()\n\
                     \main = putStrLn \"Hello, Idris!\""
          expected = "Hello, Idris!\n"
      actual <- run code
      actual `shouldBe` expected

run :: String -> IO String
run code = do
  writeFile input code
  ci <- compileCodegenInfo input output
  codegenCil ci
  ilasm output
  mono exe
  where input  = "/tmp/cil-test.idr"
        output = "/tmp/cil-test.il"
        exe    = "/tmp/cil-test.exe"

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

ilasm :: String -> IO ()
ilasm ilFile = readProcess "ilasm" [ilFile] "" >>= (\_ -> return ())

mono :: String -> IO String
mono exe = readProcess "mono" [exe] ""
