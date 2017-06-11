module IRTS.Cil.VersionParser
  ( parseVersion
  , parseAssemblyRef
  ) where

import Language.Cil (AssemblyRef(..), Version)
import Text.Trifecta hiding (dot)

parseVersion :: String -> Version
parseVersion s =
  case parseString (version <* eof) mempty s of
    Success v -> v
    Failure e -> error $ "Failed to parse version number: " ++ show (_errDoc e)

parseAssemblyRef :: String -> AssemblyRef
parseAssemblyRef s =
  case parseString (assemblyRef <* eof) mempty s of
    Success v -> v
    Failure e -> error $ "Failed to parse version number: " ++ show (_errDoc e)

assemblyRef :: CharParsing m => m AssemblyRef
assemblyRef = AssemblyRef <$> assemblyName <* assemblyRefSep <*> version <* assemblyRefSep <*> pubKeyToken
  where
    assemblyName = some (choice [alphaNum, dot])
    assemblyRefSep = many space *> char ',' <* many space
    pubKeyToken = some (choice [hexDigit, space])

version :: CharParsing m => m Version
version = (,,,) <$> int <* dot <*> int <* dot <*> int <* dot <*> int

dot :: CharParsing m => m Char
dot = char '.'

int :: CharParsing m => m Int
int = read <$> many digit
