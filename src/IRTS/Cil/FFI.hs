module IRTS.Cil.FFI (parseAssemblyQualifiedName) where

import Text.ParserCombinators.Parsec

parseAssemblyQualifiedName :: String -> Either ParseError (String, String, String)
parseAssemblyQualifiedName = parse assemblyQualifiedName "qualified name"

assemblyQualifiedName :: GenParser Char st (String, String, String)
assemblyQualifiedName = do
  char '['
  asm <- many (noneOf "]") <?> "assembly name"
  char ']'
  typeName <- manyTill anyChar (string "::") <?> "type name"
  methodName <- many1 anyChar <?> "method name"
  return (asm, typeName, methodName)
