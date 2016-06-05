module Main where

import qualified Spec
import           System.IO
import           Test.Hspec.Formatters
import           Test.Hspec.Runner

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
