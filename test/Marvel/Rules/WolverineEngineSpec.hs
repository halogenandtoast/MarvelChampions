module Marvel.Rules.WolverineEngineSpec (spec) where

import Marvel.Card.AttachmentHeroAbilities
import Marvel.Cost qualified as Cost
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Matchers (IdentityMatcher (IdentityWithId))
import Marvel.Window qualified as W
import TestImport

spec :: Spec
spec = describe "Wolverine engine mechanics" do
  it "ignores only the chosen physical card's resource cost and completes normal play" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      pass
      clearHand p
      swing <- putInHand p "01005"
      canPlayCardIgnoringResourceCost p swing `shouldReturn` True
      runNow [PlayCardIgnoringResourceCost p swing]
      vid <- villainIdNow
      choose "Swinging Web Kick target" (isTargetChoice $ VillainRef vid)
      pass
      villainDamageNow `shouldReturn` 8
      handOf p `shouldReturn` []

  it "does not let ignored costs bypass a card's play restrictions" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      swing <- putInHand p "01005"
      canPlayCardIgnoringResourceCost p swing `shouldReturn` False

  it "can pay damage in a continuation and runs that continuation afterwards" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      messages <- Lifted.runQueueT $ Lifted.capture $ Lifted.payCostThen p (IdentityRef p) (Cost.DamageCost 2) [IdentityMessage p (IdentityHealed 1)]
      resolve messages
      damageOf p `shouldReturn` 1

  it "cannot pay a take-damage cost through tough" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      resolve [IdentityMessage p IdentityBecomeTough]
      Cost.canAffordCost p (IdentityRef p) (Cost.DamageCost 2) `shouldReturn` False

  it "distinguishes lethal enemy attacks from unrelated damage" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      vid <- villainIdNow
      let matcher = W.IdentityWouldBeDefeatedByEnemyAttack (IdentityWithId p)
          matches source damage = W.windowMatches matcher (W.Window W.Would $ W.IdentityDefeatFromDamage p source damage) (IdentityRef p)
      matches (VillainRef vid) (toDamage 10 FromAttack) `shouldReturn` True
      matches (VillainRef vid) (toDamage 10 FromAbility) `shouldReturn` False
      matches GameRef (toDamage 10 FromAttack) `shouldReturn` False

  it "separates recovery interrupts from recovery responses" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      let before = W.Window W.When $ W.UsedRecovery p
          after = W.Window W.After $ W.UsedRecovery p
      W.windowMatches (W.UsesRecoveryAt W.When $ IdentityWithId p) before (IdentityRef p) `shouldReturn` True
      W.windowMatches (W.UsesRecovery $ IdentityWithId p) before (IdentityRef p) `shouldReturn` False
      W.windowMatches (W.UsesRecovery $ IdentityWithId p) after (IdentityRef p) `shouldReturn` True

  it "classifies printed attachment Hero Action and Hero Response text" do
    (hasPrintedHeroActionOrResponse "01100" `shouldBe` True :: IO ())
    hasPrintedHeroActionOrResponse "35036" `shouldBe` True
    hasPrintedHeroActionOrResponse "35030" `shouldBe` False
