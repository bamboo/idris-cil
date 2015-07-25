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

data Opts = Opts { inputs :: [FilePath]
                 , output :: FilePath }

main :: IO ()
main = do
  opts <- getOpts
  if null (inputs opts)
    then showUsage
    else runMain (cilMain opts)

getOpts :: IO Opts
getOpts = liftM (process (Opts [] "a.il")) getArgs
  where process opts ("-o":o:xs) = process (opts { output = o }) xs
        process opts (x:xs)      = process (opts { inputs = x:inputs opts }) xs
        process opts []          = opts

showUsage :: IO ()
showUsage = do
  putStrLn "Usage: idris-cil <ibc-files> [-o <output-file>]"
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
