module Marvel.Rules.WhenRevealedSpec (spec) where

import Marvel.Card.WhenRevealed (hasWhenRevealed)
import Test.Hspec qualified as Hspec
import TestImport

spec :: Spec
spec = describe "When Revealed ability labels" do
  it "does not mistake a quoted ability name for an ability on the card" do
    hasWhenRevealed "02049" `Hspec.shouldBe` False
    hasWhenRevealed "16108" `Hspec.shouldBe` False

  it "recognizes actual ability labels on attachments and obligations" do
    hasWhenRevealed "21153" `Hspec.shouldBe` True
    hasWhenRevealed "21178" `Hspec.shouldBe` True
    hasWhenRevealed "11019" `Hspec.shouldBe` True
    hasWhenRevealed "11021" `Hspec.shouldBe` True
