module Marvel.Rules.UpgradePlaySpec (spec) where

import Marvel.Ability (ability, passesCriteria, passesCardCriteria)
import Marvel.Ability.Types (AbilityType (Action))
import Marvel.Criteria
import Marvel.Cost.Types
import Marvel.Matchers
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Payment (Payment (..))
import Marvel.Query
import Marvel.Trait
import Marvel.Window qualified as W
import TestImport

spec :: Spec
spec = describe "upgrade play and negated criteria" do
  it "a negated ally-exists condition changes when a non-Guardian ally enters" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      let condition = NotCriteria (AllyExists $ AllyControlledBy (IdentityWithId p) <> NotAlly (AllyWithTrait Guardian))
          ab = ability p 1 Action condition NoCost (Label "$label.done" [])
      passesCriteria p ab `shouldReturn` True
      card <- putInHand p "01065"
      passesCardCriteria p card condition `shouldReturn` True
      _ <- putIntoPlay p "01051"
      passesCriteria p ab `shouldReturn` False
      passesCardCriteria p card condition `shouldReturn` False

  it "matches the attached ally rather than the upgrade's controller" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      ally <- putIntoPlay other "01051"
      suit <- putInHand p "12018"
      energy <- putInHand p "01088"
      playOn suit (allyIdOf ally) [energy]
      selectList (UpgradeOnAlly $ AllyControlledBy $ IdentityWithId other) `shouldReturn` [upgradeIdOf suit]
      selectList (UpgradeOnAlly $ AllyControlledBy $ IdentityWithId p) `shouldReturn` []

  it "snapshots upgrade-on-ally timing even after the ally and upgrade leave play" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- putIntoPlay p "01051"
      suit <- putInHand p "12018"
      energy <- putInHand p "01088"
      playOn suit (allyIdOf ally) [energy]
      let window = W.Window W.After $ W.UpgradePlayedOnAlly p (upgradeIdOf suit) (allyIdOf ally)
      resolve [RemoveFromPlay $ AllyRef $ allyIdOf ally]
      W.windowMatches (W.PlaysUpgradeOnAlly W.After $ IdentityWithId p) window GameRef `shouldReturn` True
      W.windowMatches (W.PlaysUpgradeOnAlly W.When $ IdentityWithId p) window GameRef `shouldReturn` False

  it "records payer and attachment target when a card is played, without misclassifying an ally-targeted event" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- putIntoPlay p "01051"
      suit <- putInHand p "12018"
      event <- putInHand p "01086"
      let target = TargetPayment (AllyRef $ allyIdOf ally)
          playedUpgrade = W.UpgradePlayedOnAlly p (upgradeIdOf suit) (allyIdOf ally)
          containsUpgrade = any (\case CheckWindows ws -> any ((== playedUpgrade) . W.windowType) ws; _ -> False)
      upgradeMessages <- Lifted.runQueueT $ Lifted.capture $ Lifted.cardPlayed p suit target Nothing
      eventMessages <- Lifted.runQueueT $ Lifted.capture $ Lifted.cardPlayed p event target Nothing
      liftIO $ containsUpgrade upgradeMessages `shouldBe` True
      liftIO $ containsUpgrade eventMessages `shouldBe` False
