module Marvel.Rules.GenerationXSpec (spec) where

import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Modifier
import Marvel.Trait qualified as Trait
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

schemeOf :: PlayerCard -> SideSchemeId
schemeOf = SideSchemeId . unCardId . pcCardId

isAllyBasicThwart :: PlayerCard -> Choice -> Bool
isAllyBasicThwart card = \case
  UseAbility ability -> abilitySource ability == toSource (allyIdOf card) && abilityIndex ability == 300
  _ -> False

giveModifiers :: IdentityId -> EntityMatcher -> [Modifier] -> GameAppT ()
giveModifiers p matcher modifiers = do
  (_, messages) <- L.captureQueueT $ L.lastingModifiers p matcher modifiers True
  runNow messages

spec :: Spec
spec = describe "Generation X conditional THW" do
  it "grants an X-Men hero one THW against Generation X" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putIntoPlay p "47016"
      let scheme = schemeOf card
      choose "basic thwart" (isHeroThwart p)
      choose "Generation X" (isTargetChoice scheme)
      pass
      sideSchemeThreat scheme `shouldReturn` 1

  it "applies its THW before clamping penalties at zero" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putIntoPlay p "47016"
      let scheme = schemeOf card
      giveModifiers p (IdentityEntity $ IdentityWithId p) [ThwartPenalty 2]
      choose "basic thwart" (isHeroThwart p)
      choose "Generation X" (isTargetChoice scheme)
      pass
      sideSchemeThreat scheme `shouldReturn` 3

  it "does not add its THW bonus when an ally thwarts using ATK" do
    tg <- testGame ["jubilee", "spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putIntoPlay p "47016"
      let scheme = schemeOf card
      giveModifiers p (SideSchemeEntity $ SideSchemeWithId scheme) [ThwartWithAttack]
      wolverine <- putIntoPlay p "47002"
      choose "Wolverine thwarts" (isAllyBasicThwart wolverine)
      choose "Generation X" (isTargetChoice scheme)
      choose "use ATK" (isLabel "$label.thwartWithAtk")
      pass
      sideSchemeThreat scheme `shouldReturn` 3

  it "adds one point to a divided THW pool when Generation X is selected later" do
    tg <- testGame ["wasp", "jubilee"]
    inGame tg do
      let p = player1 tg
      clearHand p
      changeFormTo p C
      pass
      giveModifiers p (IdentityEntity $ IdentityWithId p) [TraitModifier Trait.XMen]
      card <- putIntoPlay p "47016"
      let scheme = schemeOf card
      placeMainThreat 4
      before <- mainSchemeThreatNow
      choose "Giant Wasp thwarts" (isHeroThwart p)
      choose "divide threat" (isLabel "$label.divideThreat")
      chooseMainScheme
      choose "Generation X" (isTargetChoice scheme)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 2
      sideSchemeThreat scheme `shouldReturn` 5
