module IRTS.Cil.FFI (parseAssemblyQualifiedName) where

import Text.ParserCombinators.Parsec

parseAssemblyQualifiedName :: String -> Either ParseError (String, String, String)
parseAssemblyQualifiedName = parse assemblyQualifiedName "foreign name"

assemblyQualifiedName :: Parser (String, String, String)
assemblyQualifiedName = do
  char '['
  asm <- many (noneOf "]") <?> "assembly name"
  char ']'
  typeName <- anyChar `manyTill` string "::" <?> "type name"
  methodName <- many1 anyChar <?> "method name"
  return (asm, typeName, methodName)
