module IRTS.Cil.CaseDispatchSpec where

import IRTS.Cil.CaseDispatch

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty

import Control.Arrow ((&&&))

spec :: Spec
spec = do
  dispatchStrategyForSpec
  isDenseSpec


dispatchStrategyForSpec :: Spec
dispatchStrategyForSpec =

  describe "dispatchStrategyFor" $ do

    it "should return JumpTable for dense sequence" $
      let
        denseSeq = (id &&& id) <$> [0..5]
      in
        dispatchStrategyFor denseSeq
          `shouldBe` JumpTable (fmap Entry <$> denseSeq)

    it "should return JumpTable without gaps" $
        let
          seq = [(0, 0), (2, 2), (4, 4)]
        in
          dispatchStrategyFor seq
            `shouldBe` JumpTable [(0, Entry 0), (1, DefaultEntry), (2, Entry 2), (3, DefaultEntry), (4, Entry 4)]

    it "should return LinearSearch for sparse sequence" $
      let
        sparseSeq = [(0, 0), (99, 99)]
      in
        dispatchStrategyFor sparseSeq
          `shouldBe` LinearSearch sparseSeq


isDenseSpec :: Spec
isDenseSpec =

  describe "isDense" $ do

    it "should check for ascending order" $
      isDense [2, 1, 0] `shouldBe` False

    it "should allow for gaps" $
      isDense [0, 2, 4] `shouldBe` True
