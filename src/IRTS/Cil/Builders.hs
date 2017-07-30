module IRTS.Cil.Builders where

import Language.Cil

privateSealedClass :: TypeName -> Maybe TypeSpec -> [TypeSpec] -> [FieldDef] -> [MethodDef] -> [TypeDef] -> TypeDef
privateSealedClass = classDef [CaPrivate, CaSealed, CaBeforeFieldInit]

publicSealedClass :: TypeName -> Maybe TypeSpec -> [TypeSpec] -> [FieldDef] -> [MethodDef] -> [TypeDef] -> TypeDef
publicSealedClass = classDef [CaPublic, CaSealed, CaBeforeFieldInit]

publicStruct :: TypeName -> [FieldDef] -> [MethodDef] -> [TypeDef] -> TypeDef
publicStruct name = classDef [CaPublic] name (extends "[mscorlib]System.ValueType") noImplements

defaultCtorDef :: MethodDef
defaultCtorDef = Constructor [MaPublic] Void []
                   [ ldarg 0
                   , call [CcInstance] Void "" "object" ".ctor" []
                   , ret ]
