module Marvel.Rules.SpiderHamMechanicsSpec (spec) where

import Marvel.Ability (ability, AbilityType (Resource))
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Window qualified as W
import TestImport

spec :: Spec
spec = describe "Spider-Ham engine mechanics" do
  it "counts every counter of a repeatable resource ability and reserves counters owed by a card" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      resolve [PlaceCounters (IdentityRef p) 3]
      let resource = ability p 98 Resource IsSelf (IdentityCountersCost 1) (Pay $ ResourcePayment Wild)
      abilityResources resource `shouldReturn` [Wild, Wild, Wild]
      abilityResourcesWithReservedCounters 1 resource `shouldReturn` [Wild, Wild]
      abilityResourcesWithReservedCounters 4 resource `shouldReturn` []

  it "spends toon counters as a constant permission while Induced Panic blocks printed hero abilities" do
    tg <- testGame ["spider-ham"]
    let p = player1 tg
        isCounter = \case
          UseAbility a -> abilityIndex a == 990 && abilitySource a == IdentityRef p
          _ -> False
    inGame tg do
      changeForm p
      pass
      clearHand p
      resolve [PlaceCounters (IdentityRef p) 3]
      _ <- revealNow p Attachments.smInducedPanic
      pass
      event <- putInHand p "30007"
      refreshTurn
      offers (isPlay event) `shouldReturn` True
      choose "play Swinging Web Pig" (isPlay event)
      for_ [1 :: Int .. 3] $ const $ choose "spend a toon counter" isCounter
      whenM (offers isFinishPayment) $ choose "finish payment" isFinishPayment
      chooseVillain
      pass
      playerIdentityCounters <$> playerAttrs p `shouldReturn` 0
      villainDamageNow `shouldReturn` 6

  it "does not offer a second Cartoon Physics after the first leaves only one pending damage" do
    tg <- testGame ["spider-ham"]
    let p = player1 tg
    inGame tg do
      first <- putIntoPlay p "30009"
      second <- putIntoPlay p "30009"
      runNow $ damageIdentity p GameRef (toDamage 5 FromAbility)
      choose "prevent all but one damage" (isAbilityOfCard first)
      offers (isAbilityOfCard second) `shouldReturn` False
      pass
      damageOf p `shouldReturn` 1
      length <$> upgradesWith "30009" `shouldReturn` 1

  it "matches identity damage from an ability as well as from an enemy attack, but not zero damage" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      let matches damage = W.windowMatches (W.IdentityDamaged W.After $ IdentityWithId p)
            (W.Window W.After $ W.IdentityTookDamage p GameRef damage) (IdentityRef p)
      matches (toDamage 1 FromAbility) `shouldReturn` True
      matches (toDamage 2 FromAttack) `shouldReturn` True
      matches (toDamage 0 FromAbility) `shouldReturn` False

  it "distinguishes a thwart's actual removal from other threat removal and its timing" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      sid <- mainSchemeIdNow
      let scheme = SchemeMainSchemeId sid
          matcher = W.ThreatRemovedByThwart W.When (SourceIs $ IdentityRef p) AnyScheme
          matches window = W.windowMatches matcher window (IdentityRef p)
      matches (W.Window W.When $ W.ThwartRemovedThreat (IdentityRef p) scheme 1 1) `shouldReturn` True
      matches (W.Window W.After $ W.ThwartRemovedThreat (IdentityRef p) scheme 1 1) `shouldReturn` False
      matches (W.Window W.When $ W.ThreatRemovedBy (IdentityRef p) scheme 1 1) `shouldReturn` False
      matches (W.Window W.When $ W.ThwartRemovedThreat (IdentityRef p) scheme 0 0) `shouldReturn` False
