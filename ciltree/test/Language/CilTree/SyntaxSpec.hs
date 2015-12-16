module Language.CilTree.SyntaxSpec where

import Language.CilTree.Emit
import Language.CilTree.Syntax
import Test.Hspec

spec :: Spec
spec =
  describe "emit" $
    it "tryParse" $
      emit tryParse `shouldBe` tryParseCil

tryParseCil :: String
tryParseCil = "TODO"

tryParse :: MethodDef
tryParse = MethodDef signature body
  where signature = MethodRef Static (ReferenceType "" "T") "tryParse" (Type Int32) [Type String]
        body      = let result = Local (Type Int32) 1
                    in Let result (Const $ CInt32 0) $
                         Seq [ Call False
                                    (MethodRef Static Int32 "TryParse" (Type Bool) [Type String, Type (ByRef Int32)])
                                    [GetArg 0, GetAddr result]
                             , Get result ]
