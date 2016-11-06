module IRTS.Cil.VersionParser (parseVersion) where

import Language.Cil (Version)
import Text.Trifecta hiding (dot)

parseVersion :: String -> Version
parseVersion s =
  case parseString version mempty s of
    Success v -> v
    Failure e -> error $ "Failed to parse version number: " ++ show (_errDoc e)

version :: CharParsing m => m Version
version = (,,,) <$> int <* dot <*> int <* dot <*> int <* dot <*> int <* eof

dot :: CharParsing m => m Char
dot = char '.'

int :: CharParsing m => m Int
int = read <$> many digit
