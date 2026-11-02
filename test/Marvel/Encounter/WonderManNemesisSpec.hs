module Marvel.Encounter.WonderManNemesisSpec (spec) where

import Marvel.Message.Lifted qualified as Lifted
import Marvel.WonderMan.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Query
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero you = changeForm you >> pass >> clearHand you

spec :: Spec
spec = describe "Wonder Man nemesis" do
  it "Pacifism prevents Wonder Man from attacking until its alter-ego action discards it" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      obligation <- revealNow you Cards.wm58025
      pass
      offers (isHeroAttack you) `shouldReturn` False
      runNow [IdentityMessage you $ ChangedToForm B]
      choose "remove Pacifism" (isAbilityOf $ ObligationRef $ ObligationId $ unCardId $ ecCardId obligation)
      pass
      cards <- encounterDiscard
      any ((== "58025") . toCardCode) cards `shouldBe` True

  it "Scythe Strike deals two indirect damage when Grim Reaper is absent" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      card <- encounterCard Cards.wm58028
      resolve [RevealEncounterCard you card]
      damageOf you `shouldReturn` 2

  it "Death Cannot Die finds and reveals Grim Reaper when he is absent" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      card <- encounterCard Cards.wm58029
      resolve [RevealEncounterCard you card]
      selectAny (MinionIs Cards.wm58026) `shouldReturn` True
      damageOf you `shouldReturn` 2

  it "Grim Reaper deals an encounter card after defeating an ally with his attack" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      reaper <- addMinion you Cards.wm58026
      ally <- putIntoPlay you "01002"
      before <- length . (.playerIdentityEncounterCards) <$> playerAttrs you
      Lifted.runQueueT (Lifted.capture $ Lifted.enemyAttacksAlly reaper you $ allyIdOf ally) >>= resolve
      after <- length . (.playerIdentityEncounterCards) <$> playerAttrs you
      after `shouldBe` before + 1

  it "Brother vs Brother hides an unaffordable attack on Grim Reaper" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      reaper <- addMinion you Cards.wm58026
      _ <- revealNow you Cards.wm58027
      pass
      clearHand you
      choose "attack" (isHeroAttack you)
      offers (isTargetChoice reaper) `shouldReturn` False

  it "Brother vs Brother requires discarding a hand card before attacking Grim Reaper" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      reaper <- addMinion you Cards.wm58026
      _ <- revealNow you Cards.wm58027
      pass
      payment <- putInHand you "58003"
      choose "attack" (isHeroAttack you)
      choose "Grim Reaper" (isTargetChoice reaper)
      choose "discard the additional cost" (isTargetChoice payment)
      pass
      handOf you `shouldReturn` []

  it "Brother vs Brother charges an ally attack's controller one discarded hand card" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      reaper <- addMinion you Cards.wm58026
      _ <- revealNow you Cards.wm58027
      pass
      _ <- putIntoPlay you "58013"
      hawkeye <- allyIn "58013"
      payment <- putInHand you "58003"
      choose "Hawkeye attacks" (isAllyAttack hawkeye)
      choose "Grim Reaper" (isTargetChoice reaper)
      choose "discard one card" (isTargetChoice payment)
      pass
      cards <- discardOf you
      (payment `elem` cards) `shouldBe` True

  it "Brother vs Brother charges for an event attack in addition to its printed resource cost" do
    tg <- testGame ["wonder-man"]
    inGame tg do
      let you = player1 tg
      asHero you
      reaper <- addMinion you Cards.wm58026
      _ <- revealNow you Cards.wm58027
      pass
      event <- putInHand you "58004"
      resource <- putInHand you "58003"
      additional <- putInHand you "58010"
      playWith event [resource]
      choose "Grim Reaper" (isTargetChoice reaper)
      choose "discard the additional hand card" (isTargetChoice additional)
      pass
      cards <- discardOf you
      (resource `elem` cards && additional `elem` cards) `shouldBe` True

  it "Brother vs Brother contributes an amplify icon and two threat per player" do
    tg <- testGame ["wonder-man", "hercules"]
    inGame tg do
      scheme <- revealNow (player1 tg) Cards.wm58027
      pass
      sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 4
      getAmplifyCount `shouldReturn` 1
