module Main where

import Control.Monad (liftM)
import IRTS.CodegenCil
import IRTS.CodegenCommon
import IRTS.Compiler
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.REPL

import System.Environment
import System.Exit

data Opts = Opts { really :: Bool
                 , inputs :: [FilePath]
                 , output :: FilePath }

main :: IO ()
main = do
  opts <- getOpts
  if null (inputs opts)
    then showUsage
    else if not $ really opts
           then do putStrLn "This code generator is intended to be called by the Idris compiler. \
                            \Please pass Idris the '--codegen' flag to choose a backend."
                   exitSuccess
           else runMain (cilMain opts)

getOpts :: IO Opts
getOpts = liftM (process (Opts False [] "a.il")) getArgs
  where process opts ("--yes-really":xs) = process (opts { really = True }) xs
        process opts ("-o":o:xs)         = process (opts { output = o }) xs
        process opts (x:xs)              = process (opts { inputs = x:inputs opts }) xs
        process opts []                  = opts

showUsage :: IO ()
showUsage = do
  putStrLn "CIL code generator which is intended to be called by the compiler, not by a user."
  putStrLn "To call the code generator manually, pass the --yes-really option.\n"
  putStrLn "Usage: idris-codegen-cil --yes-really <ibc-files> [-o <output-file>]"
  exitSuccess

codegenInfoFrom :: Opts -> Idris CodegenInfo
codegenInfoFrom opts = do
  elabPrims
  loadInputs (inputs opts) Nothing
  mainProg <- elabMain
  compile (Via "cil") (output opts) (Just mainProg)

cilMain :: Opts -> Idris ()
cilMain opts = do
  ci <- codegenInfoFrom opts
  runIO $ codegenCil ci
