{-# LANGUAGE OverloadedStrings #-}
module IRTS.Cil.FFI
       ( parseAssemblyQualifiedName
       , foreignTypeToCilType
       ) where

import           Data.Maybe
import           Data.Text
import           Text.ParserCombinators.Parsec
import qualified Data.HashMap.Strict as HM

import           IRTS.Lang (FDesc(..))
import           Idris.Core.TT (Name(..))
import           Language.Cil (PrimitiveType(..))

parseAssemblyQualifiedName :: String -> Either ParseError (Bool, String, String, String)
parseAssemblyQualifiedName = parse assemblyQualifiedName "foreign name"

assemblyQualifiedName :: Parser (Bool, String, String, String)
assemblyQualifiedName = do
  maybeInstance <- optionMaybe (string "instance")
  spaces
  char '['
  asm <- many (noneOf "]") <?> "assembly name"
  char ']'
  typeName <- anyChar `manyTill` string "::" <?> "type name"
  methodName <- many1 anyChar <?> "method name"
  return (isJust maybeInstance, asm, typeName, methodName)

foreignTypeToCilType :: FDesc -> PrimitiveType
foreignTypeToCilType (FApp t _) = foreignType t
foreignTypeToCilType (FCon t)   = foreignType t

foreignType :: Name -> PrimitiveType
foreignType (UN typeName) =
  fromMaybe
    (error $ "Unsupported foreign type: " ++ unpack typeName)
    (HM.lookup typeName foreignTypes)

foreignTypes :: HM.HashMap Text PrimitiveType
foreignTypes = HM.fromList [("C_IntT", Int32)
                           ,("C_Str", String)
                           ]
