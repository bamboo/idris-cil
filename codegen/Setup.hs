import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup

import System.Exit
import System.Process

import Data.Monoid ((<>))

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { postBuild = installRtsPackage }

installRtsPackage :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
installRtsPackage _ _ _ _ =
  traceProcess (proc "idris" ["--install", "cil.ipkg"]) { cwd = Just "rts" }

traceProcess :: CreateProcess -> IO ()
traceProcess args = do
  (code, stdout, stderr) <- readCreateProcessWithExitCode args ""
  case code of
    ExitFailure _ -> error $ stdout <> stderr
    _             -> return ()
