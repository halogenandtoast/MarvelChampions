module Marvel.Encounter.ShadowlandSpellsSpec (spec) where

import Marvel.FearNoEvil.Cards qualified as FNE
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Query
import Marvel.Shadowland.Cards qualified as Cards
import TestImport

spellFixture :: TestGame -> GameAppT (IdentityId,VillainId,UpgradeId)
spellFixture tg = do
  let you = player1 tg
  vid <- villainIdNow
  resolve [ReplaceVillain vid "65057" Advanced]
  card <- encounterCard Cards.street65072a
  resolve [PutEncounterCardIntoPlay you card]
  uid <- selectJust $ UpgradeNamed "Creeping Corruption"
  pure (you,vid,uid)

spec :: Spec
spec = describe "Shadowland spell behaviors" do
  for_ [(Cards.street65064,StunnedStatus),(Cards.street65065,ConfusedStatus)] $ \(def,status) ->
    it ("rejecting " <> show (toCardCode def) <> " takes four damage, gives its status, and removes one corruption") do
      tg <- testGame ["spider-man"]
      inGame tg do
        (you,_,uid) <- spellFixture tg
        resolve [PlaceCounters (UpgradeRef uid) 2]
        card <- revealNow you def
        choose "reject the spell" (isLabel "$label.slResistSpell")
        pass
        damageOf you `shouldReturn` 4
        getCountersOn uid `shouldReturn` 1
        getCharacterStatusCount (IdentityCharacter you) status `shouldReturn` 1
        selectAny (AttachmentWithId $ AttachmentId $ unCardId $ ecCardId card) `shouldReturn` False

  it "Mark of the Wicked adds corruption after its attached hero makes a basic attack" do
    tg <- testGame ["spider-man"]
    inGame tg do
      (you,vid,uid) <- spellFixture tg
      changeForm you
      clearHand you
      card <- revealNow you Cards.street65064
      choose "accept the mark" (isLabel "$label.slAcceptSpell")
      pass
      let aid = AttachmentId $ unCardId $ ecCardId card
      choose "make a basic attack" (isHeroAttack you)
      choose "attack Krahllak" (isTargetChoice vid)
      choose "resolve the mark's response" (isAbilityOf aid)
      pass
      getVillainHitPoints vid `shouldReturn` 16
      getCountersOn uid `shouldReturn` 1

  it "Pull of Power adds corruption after a basic thwart" do
    tg <- testGame ["spider-man"]
    inGame tg do
      (you,_,uid) <- spellFixture tg
      changeForm you
      clearHand you
      card <- revealNow you Cards.street65065
      choose "accept Pull of Power" (isLabel "$label.slAcceptSpell")
      pass
      let aid = AttachmentId $ unCardId $ ecCardId card
      scheme <- selectJust EncounterMainScheme
      resolve =<< L.runQueueT (L.capture $ L.placeThreatOn GameRef scheme 3)
      choose "make a basic thwart" (isHeroThwart you)
      choose "thwart the main scheme" (isTargetChoice scheme)
      choose "resolve the spell's response" (isAbilityOf aid)
      pass
      mainSchemeThreatNow `shouldReturn` 2
      getCountersOn uid `shouldReturn` 1

  it "Fear Projection's hero action discards it for two corruption" do
    tg <- testGame ["spider-man"]
    inGame tg do
      (you,_,uid) <- spellFixture tg
      changeForm you
      card <- revealNow you Cards.street65063
      pass
      let aid = AttachmentId $ unCardId $ ecCardId card
      choose "take corruption to discard Fear Projection" (isAbilityOf aid)
      pass
      getCountersOn uid `shouldReturn` 2
      selectAny (AttachmentWithId aid) `shouldReturn` False

  it "Fear Projection's alter-ego action pays three mental resources and removes one corruption" do
    tg <- testGame ["spider-man"]
    inGame tg do
      (you,_,uid) <- spellFixture tg
      clearHand you
      resolve [PlaceCounters (UpgradeRef uid) 2]
      card <- revealNow you Cards.street65063
      pass
      genius <- putInHand you "01089"
      sense <- putInHand you "01004"
      let aid = AttachmentId $ unCardId $ ecCardId card
      choose "pay to discard Fear Projection" (isAbilityOf aid)
      payWith [genius,sense]
      pass
      getCountersOn uid `shouldReturn` 1
      selectAny (AttachmentWithId aid) `shouldReturn` False

  for_ [False,True] $ \unpreventable ->
    it ("Dark Shroud resolves corruption and discard while respecting unpreventable=" <> show unpreventable) do
      tg <- testGame ["spider-man"]
      inGame tg do
        (you,vid,uid) <- spellFixture tg
        changeForm you
        clearHand you
        card <- revealNow you Cards.street65062
        pass
        let aid = AttachmentId $ unCardId $ ecCardId card
            damage = (if unpreventable then withUnpreventable else id) $ toDamage 4 $ FromPlayerAttack you
        _ <- setEncounterDeck (replicate 3 FNE.street60188) []
        runNow [VillainMessage vid $ VillainDamaged (IdentityRef you) damage]
        choose "resolve Dark Shroud" (isAbilityOf aid)
        pass
        getCountersOn uid `shouldReturn` 1
        selectAny (AttachmentWithId aid) `shouldReturn` False
        getVillainHitPoints vid `shouldReturn` (if unpreventable then 14 else 18)
