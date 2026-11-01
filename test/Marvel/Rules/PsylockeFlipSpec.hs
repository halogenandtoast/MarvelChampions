module Marvel.Rules.PsylockeFlipSpec (spec) where

import Marvel.Matchers
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Query
import TestImport

psiControl :: IdentityId -> Choice -> Bool
psiControl you = \case
  UseAbility ability -> abilitySource ability == IdentityRef you && abilityIndex ability == 1
  _ -> False

spec :: Spec
spec = describe "Psylocke flip legality" do
  it "offers Psi-Energy Control when one of her weapons can flip" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let you = player1 tg
      clearHand you
      changeForm you
      pass
      choose "make a basic attack" (isHeroAttack you)
      offers (psiControl you) `shouldReturn` True

  it "does not offer Psi-Energy Control when Body Swapped locks both weapons" do
    tg <- testGame ["psylocke"]
    inGame tg do
      let you = player1 tg
      clearHand you
      card <- encounterCard Obligations.bodySwapped
      resolve [RevealEncounterCard you card]
      length <$> upgradesWith "41002b" `shouldReturn` 2
      selectList UpgradeCanFlipPsiEnergy `shouldReturn` []
      changeForm you
      pass
      choose "make a basic attack" (isHeroAttack you)
      offers (psiControl you) `shouldReturn` False
      chooseVillain
      villainDamageNow `shouldReturn` 3
