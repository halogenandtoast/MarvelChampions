module Marvel.Rules.HeroPackMechanicsSpec (spec) where

import Marvel.Cost.Types
import TestImport
import Test.Hspec qualified as H

spec :: Spec
spec = describe "Hero pack resource costs" do
  it "requires two matching resource types for Grounded" do
    coversSameType 2 [Energy, Physical] `H.shouldBe` False
    coversSameType 2 [Energy, Energy] `H.shouldBe` True
    coversSameType 2 [Mental, Mental] `H.shouldBe` True
    coversSameType 2 [Physical, Physical] `H.shouldBe` True

  it "permits a wild to pay a matching resource requirement" do
    coversSameType 2 [Wild, Mental] `H.shouldBe` True
    coversSameType 2 [Wild, Wild] `H.shouldBe` True
    coversSameType 2 [Energy] `H.shouldBe` False

  it "retains matching-resource requirements inside combined costs" do
    sameResourcesNeeded (Costs [ResourceCost Nothing, SameResourcesCost 2]) `H.shouldBe` 2
    length (costResources $ SameResourcesCost 2) `H.shouldBe` 2
