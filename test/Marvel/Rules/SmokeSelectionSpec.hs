module Marvel.Rules.SmokeSelectionSpec (spec) where

import Prelude
import SmokeSelection
import Test.Hspec

spec :: Spec
spec = describe "focused smoke selection" do
  it "keeps every relevant case and adds exactly 50 distinct unrelated cases" do
    let selected = selectSmokeCases 17 50 (<= 20) [1..200 :: Int]
    filter (<= 20) selected `shouldBe` [1..20]
    length (filter (> 20) selected) `shouldBe` 50
    length selected `shouldBe` 70

  it "replays a sample by seed and changes it for a different seed" do
    let select seed = selectSmokeCases seed 50 (const False) [1..200 :: Int]
    select 17 `shouldBe` select 17
    select 17 `shouldNotBe` select 18

  it "takes every remaining case when fewer than 50 exist, without duplicates" do
    selectSmokeCases 17 50 (<= 2) [1,2,3,3,4 :: Int] `shouldBe` [1,2,3,4]

  it "preserves catalogue order and includes relevant cases even with no extra sample" do
    selectSmokeCases 17 0 even [5,4,3,2,1 :: Int] `shouldBe` [4,2]
