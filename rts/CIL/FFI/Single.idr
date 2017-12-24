module CIL.FFI.Single

import public CIL.FFI

%access public export

SingleTy : CILTy
SingleTy = CILTyVal "" "float32"

Single : Type
Single = CIL SingleTy

%extern prim__singleAdd : Single -> Single -> Single
%extern prim__singleSub : Single -> Single -> Single
%extern prim__singleMul : Single -> Single -> Single
%extern prim__singleFromInteger : Integer -> Single
%extern prim__singleFromInt : Int -> Single
%extern prim__singleFromDouble : Double -> Single
%extern prim__singleNeg : Single -> Single
%extern prim__singleAbs : Single -> Single
%extern prim__singleDiv : Single -> Single -> Single
%extern prim__singleCompare : Single -> Single -> Int
%extern prim__singleMax : Single -> Single -> Single
%extern prim__singleMin : Single -> Single -> Single
%extern prim__singleShow : Single -> String

implementation Num Single where
  (+) = prim__singleAdd
  (*) = prim__singleMul
  fromInteger = prim__singleFromInteger

implementation Neg Single where
  negate = prim__singleNeg
  (-) = prim__singleSub

implementation Abs Single where
  abs = prim__singleAbs

implementation Fractional Single where
  (/) = prim__singleDiv

implementation Eq Single where
  (==) x y = prim__singleCompare x y == 0

implementation Ord Single where
  compare x y =
    let ord = prim__singleCompare x y
    in if (ord == 0) then EQ else (if ord < 0 then LT else GT)
  (<) x y = prim__singleCompare x y < 0
  (>) x y = prim__singleCompare x y > 0
  (<=) x y = prim__singleCompare x y <= 0
  (>=) x y = prim__singleCompare x y >= 0
  max = prim__singleMax
  min = prim__singleMin

implementation Show Single where
  show = prim__singleShow

implementation Cast Double Single where
  cast = prim__singleFromDouble

implementation Cast Integer Single where
  cast = prim__singleFromInteger

implementation Cast Int Single where
  cast = prim__singleFromInt

single : Double -> Single
single = cast
