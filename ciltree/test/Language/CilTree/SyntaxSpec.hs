module Language.CilTree.SyntaxSpec where


import           Data.Function (on)
import qualified Data.Text as T
import           Language.CilTree.Emit
import           Language.CilTree.Syntax
import           Test.Hspec (Spec, describe, parallel, it)
import           Test.Hspec.Expectations.Pretty


spec :: Spec
spec =
  describe "emit" $ parallel $ do
    it "tryParse" $
      cilForMember tryParse `shouldBeSameTextAs` tryParseCil
    it "ifEquals" $
      cilForMember ifEquals `shouldBeSameTextAs` ifEqualsCil
    it "assembly with two types" $
      cilFor assemblyWithTwoTypes `shouldBeSameTextAs` assemblyWithTwoTypesCil


shouldBeSameTextAs :: String -> String -> Expectation
shouldBeSameTextAs = shouldBe `on` textForComparison


textForComparison :: String -> [T.Text]
textForComparison = map T.strip . T.lines . T.strip . T.pack


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


tryParse :: MemberDefinition
tryParse = MethodDefinition signature body
  where signature = MethodRef Static (ReferenceType "" "T") "tryParse" (Type Int32) [Type String]
        body      = let result = Local (Type Int32) 1
                    in Let result (Const $ CInt32 0) $
                         Seq [ Call False Nothing
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


ifEquals :: MemberDefinition
ifEquals = MethodDefinition signature body
  where signature = MethodRef Static (ReferenceType "" "T") "ifEquals" (Type String) [Type person]
        person    = ReferenceType "" "Person"
        body      = If (Binary Object Eql (GetArg 0) Null)
                       (Const (CStr ""))
                       (GetField (Just (GetArg 0)) (FieldRef Instance person "name" (Type String)))


assemblyWithTwoTypes :: AssemblyDefinition
assemblyWithTwoTypes = AssemblyDefinition "AssemblyWithTwoTypes" [foo, bar]
  where foo = ClassDefinition "Foo" [ctor, equals]
        bar = ClassDefinition "Bar" [ctor, toString]
        ctor = ConstructorDefinition Instance [] ctorBody
        ctorBody = Call False (Just this) (MethodRef Instance Object ".ctor" (Type Void) []) []
        equals = MethodDefinition equalsSignature equalsBody
        equalsSignature = MethodRef Instance Object "Equals" (Type Bool) [Type Object, Type Object]
        equalsBody = Const (CBool False)
        toString = MethodDefinition toStringSignature toStringBody
        toStringSignature = MethodRef Instance Object "ToString" (Type String) []
        toStringBody = Const (CStr "no string")
        this = GetArg 0


assemblyWithTwoTypesCil :: String
assemblyWithTwoTypesCil = "\
  \.assembly AssemblyWithTwoTypes {}\n\
  \.class Foo\n\
  \{\n\
    \.method void .ctor() cil managed\n\
    \{\n\
        \ldarg.0\n\
        \call void object::.ctor()\n\
        \ret\n\
    \}\n\
    \.method assembly bool Equals(object p0, object p1) cil managed\n\
    \{\n\
      \ldc.i4.0\n\
      \ret\n\
    \}\n\
  \}\n\
  \\n\
  \.class Bar\n\
  \{\n\
    \.method void .ctor() cil managed\n\
    \{\n\
      \ldarg.0\n\
      \call void object::.ctor()\n\
      \ret\n\
    \}\n\
    \.method assembly string ToString() cil managed\n\
    \{\n\
      \ldstr \"no string\"\n\
      \ret\n\
    \}\n\
  \}"
