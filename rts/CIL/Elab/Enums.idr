module CIL.Elab.Enums

import CIL.FFI

import Language.Reflection.Utils

fullyQualify : TTName -> Elab TTName
fullyQualify name = NS name <$> currentNamespace

||| ~name : Type
||| ~name = TEnum ~cilTy ~reprTy
defEnumType : TTName -> Raw -> Raw -> Elab ()
defEnumType name cilTy reprTy = do
  declareType $
    Declare name [] RType
  defineFunction $
    DefineFun name [MkFunClause (Var name) `(CILEnum ~cilTy ~reprTy)]

||| ~fieldName : ~typeName
||| ~fieldName = theEnum {cilTy=~cilTy} {reprTy=~reprTy} ~fieldValue
defEnumField : TTName -> Raw -> Raw -> TTName -> Raw -> Elab ()
defEnumField typeName cilTy reprTy fieldName fieldValue = do
  fqn <- fullyQualify fieldName
  declareType $
    Declare fqn [] (Var typeName)
  defineFunction $
    DefineFun fqn [MkFunClause (Var fqn)
                              `(theEnum {cilTy=~cilTy} {reprTy=~reprTy} ~fieldValue)]

constOf : NativeTy -> Integer -> Elab Const
constOf IT32 = pure . B32 . fromInteger
constOf IT16 = pure . B16 . fromInteger
constOf ty   = const $ fail [TextPart ("Invalid enum native type: " ++ show ty)]

defEnumFields : TTName -> (cilTy : Raw) -> Raw -> NativeTy -> List (TTName, Integer) -> Elab ()
defEnumFields name cilTy reprTy nativeTy fields =
  for_ {b = Unit} fields $ \(fieldName, value) => do
    nativeValue <- constOf nativeTy value
    defEnumField name cilTy reprTy fieldName (RConstant nativeValue)

export
defEnum : TTName -> (cilTy : Raw) -> Raw -> List (TTName, Integer) -> Elab ()
defEnum name cilTy reprTy@(RConstant (AType (ATInt (ITFixed nativeTy)))) fields = do
  fqn <- fullyQualify name
  defEnumType fqn cilTy reprTy
  defEnumFields fqn cilTy reprTy nativeTy fields
defEnum _ _ reprTy _ =
  fail [TextPart ("Invalid enum representation type (must be either Bits16 or Bits32): " ++ show reprTy)]


syntax cilEnum {x} [cilTy] [reprTy] [fs] = defEnum `{{x}} `(cilTy) `(reprTy) fs

syntax cilField {x} [value] = (`{{x}}, value)
