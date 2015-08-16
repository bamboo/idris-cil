module IRTS.Cil.FFI (parseAssemblyQualifiedName) where

import Data.List (intercalate)
import Text.ParserCombinators.Parsec

parseAssemblyQualifiedName :: String -> Either ParseError (String, String, String)
parseAssemblyQualifiedName = parse assemblyQualifiedName "qualified name"

assemblyQualifiedName :: GenParser Char st (String, String, String)
assemblyQualifiedName = do
  char '['
  asm <- many (noneOf "]")
  char ']'
  ns <- many (noneOf ".") `sepBy` char '.'
  return (asm, intercalate "." $ init ns, last ns)
