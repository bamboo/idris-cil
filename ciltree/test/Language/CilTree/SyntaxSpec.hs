module Language.CilTree.SyntaxSpec where

import           Data.Function (on)
import qualified Data.Text as T
import           Language.CilTree.Emit
import           Language.CilTree.Syntax
import           Test.Hspec

spec :: Spec
spec =
  describe "emit" $ do
    it "tryParse" $
      cilFor tryParse `shouldBeSameTextAs` tryParseCil
    it "ifEquals" $
      cilFor ifEquals `shouldBeSameTextAs` ifEqualsCil

shouldBeSameTextAs :: String -> String -> Expectation
shouldBeSameTextAs = shouldBe `on` textForComparison

textForComparison :: String -> [T.Text]
textForComparison = map T.strip . T.lines . T.pack

tryParseCil :: String
tryParseCil = "\
  \.method static assembly int32 tryParse(string p0) cil managed\n\
  \   {\n\
  \       .locals init (\n\
  \            int32 l1)\n\
  \       ldc.i4.0 \n\
  \       stloc l1\n\
  \       ldarg.0 \n\
  \       ldloca l1\n\
  \       call bool int::TryParse(string, int32&)\n\
  \       pop\n\
  \       ldloc l1\n\
  \       ret\n\
  \    }\n"

tryParse :: MethodDef
tryParse = MethodDef signature body
  where signature = MethodRef Static (ReferenceType "" "T") "tryParse" (Type Int32) [Type String]
        body      = let result = Local (Type Int32) 1
                    in Let result (Const $ CInt32 0) $
                         Seq [ Call False
                                    (MethodRef Static Int32 "TryParse" (Type Bool) [Type String, Type (ByRef Int32)])
                                    [GetArg 0, GetAddr result]
                             , Get result ]

ifEqualsCil :: String
ifEqualsCil = "\
  \.method static assembly string ifEquals(class Person p0) cil managed\n\
  \   {\n\
  \       ldarg.0 \n\
  \       ldnull \n\
  \       ceq \n\
  \       brtrue L1\n\
  \       ldarg.0 \n\
  \       ldfld string Person::name \n\
  \       br L2 \n\
  \    L1: \n\
  \       ldstr \"\"\n\
  \    L2: \n\
  \       ret\n\
  \    }\n"

ifEquals :: MethodDef
ifEquals = MethodDef signature body
  where signature = MethodRef Static (ReferenceType "" "T") "ifEquals" (Type String) [Type person]
        person    = ReferenceType "" "Person"
        body      = If (Binary Object Eql (GetArg 0) Null)
                       (Const (CStr ""))
                       (GetField (GetArg 0) (FieldRef Instance person "name" (Type String)))
