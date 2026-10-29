module Marvel.Rules.AllyDiscardCostSpec (spec) where

import Marvel.ActiveCost.Types
import Marvel.Cost (canAffordCost)
import Marvel.Cost.Types
import Marvel.Matchers.Types
import Marvel.Trait (Trait (Guardian))
import Marvel.Payment.Types
import TestImport

spec :: Spec
spec = describe "discarding an ally to pay a cost" do
  it "requires an ally the payer controls" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      _ <- putIntoPlay other "01068"
      canAffordCost p GameRef (DiscardAllyCost AnyAlly) `shouldReturn` False
      canAffordCost other GameRef (DiscardAllyCost AnyAlly) `shouldReturn` True

  it "retains the ally's printed card after the chosen ally has been discarded" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- putIntoPlay p "01068"
      _ <- putIntoPlay p "01083"
      acid <- getRandom
      let cost = DiscardAllyCost AnyAlly
      -- Keep the active cost open so its retained payment can be inspected, as an
      -- event handler would inspect the payment passed to PlayedEvent.
      withGame_ $ activeCostL .~ [ActiveCost acid p ForTreachery cost NoPayment Nothing []]
      runNow [PayCost acid cost]
      choose "discard Vision" (isTargetChoice $ allyIdOf ally)
      payment <- getCurrentPayment
      paymentDiscardedAllies payment `shouldBe` [PlayerCard ally]
      map (cdCost . getCardDef) (paymentDiscardedAllies payment) `shouldBe` [Just 4]
      discardOf p >>= (`shouldSatisfy` elem ally)
      alliesWith "01068" `shouldReturn` []

  it "filters the allies that can be discarded by the cost's matcher" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- putIntoPlay p "01068"
      canAffordCost p GameRef (DiscardAllyCost $ AllyWithTrait Guardian) `shouldReturn` False
