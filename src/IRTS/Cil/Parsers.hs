module IRTS.Cil.Parsers
  ( parseVersion
  , parseAssemblyRef
  ) where

import Control.Applicative
import Language.Cil (AssemblyRef(..), Version)
import Text.Trifecta hiding (dot)

parseVersion :: String -> Version
parseVersion = parseFully version "a version number"

parseAssemblyRef :: String -> AssemblyRef
parseAssemblyRef = parseFully assemblyRef "an assembly reference"

parseFully :: Parser a -> String -> String -> a
parseFully parser description s =
  case parseString (parser <* eof) mempty s of
    Success v -> v
    Failure e -> error $ "Failed to parse `" ++ s ++ "` as " ++ description ++ ": " ++ show (_errDoc e)

assemblyRef :: CharParsing m => m AssemblyRef
assemblyRef = AssemblyRef <$> assemblyName <* comma <*> version <* comma <*> pubKeyToken <?> "assembly reference"
  where
    assemblyName = some (alphaNum <|> dot)   <?> "assembly name"
    comma        = ws *> char ',' <* ws      <?> "comma"
    pubKeyToken  = some (hexDigit <|> space) <?> "public key token"
    ws           = many space

version :: CharParsing m => m Version
version = (,,,) <$> int <* dot <*> int <* dot <*> int <* dot <*> int <?> "version number"

dot :: CharParsing m => m Char
dot = char '.' <?> "dot"

int :: CharParsing m => m Int
int = read <$> many digit <?> "integer"
