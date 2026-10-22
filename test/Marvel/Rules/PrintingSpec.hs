module Marvel.Rules.PrintingSpec (spec) where

import Marvel.Ally.Cards qualified as Cards
import TestImport

spec :: Spec
spec = describe "a card reprinted in another pack" do
  it "plays as the card it reprints, in its own printing" do
    tg <- testGame ["captain-marvel"]
    let p = player1 tg
    inGame tg do
      clearHand p
      -- Captain America's Hawkeye is the core set's
      putIntoPlay p "03012"
      hawkeye <- allyIn "03012"
      hawkeye.allyCounters `shouldBe` 4
      sameCard (allyCardDef hawkeye) Cards.hawkeyeClintBarton `shouldBe` True
