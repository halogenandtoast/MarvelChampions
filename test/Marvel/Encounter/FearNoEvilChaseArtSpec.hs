module Marvel.Encounter.FearNoEvilChaseArtSpec (spec) where

import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Query
import TestImport

spec :: Spec
spec = describe "Fear No Evil art and chase behaviors" do
  it "Art Thief searches the discard for a physical Art card and attaches it to the villain" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- villainIdNow
      (_,[art]) <- setEncounterDeck [Cards.street60188,Cards.street60188] [Cards.street60124]
      _ <- revealNow you Cards.street60126
      choose "take the discarded art" (\case CardLabel code _ -> code == "60124"; _ -> False)
      choose "resolve its attachment effect" (isAbilityOf $ AttachmentId $ unCardId $ ecCardId art)
      pass
      getAttachedCharacter (AttachmentId $ unCardId $ ecCardId art) `shouldReturn` Just (VillainCharacter vid)
      getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 1
      getEncounterDiscard >>= \cards -> liftIO $ map ecCardId cards `shouldNotContain` [ecCardId art]

  it "Tug of War accepts three physical resources and transfers art without revealing it again" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      vid <- villainIdNow
      art <- encounterCard Cards.street60123
      resolve =<< L.runQueueT (L.capture $ L.attachEncounterToCharacter art $ VillainCharacter vid)
      strength <- putInHand you "01090"
      backflip <- putInHand you "01003"
      _ <- revealNow you Cards.street60127
      choose "pay to take art" (isLabel "$label.fneTakeArt")
      payWith [strength,backflip]
      choose "take the Crown" (isTargetChoice $ AttachmentId $ unCardId $ ecCardId art)
      pass
      getAttachedCharacter (AttachmentId $ unCardId $ ecCardId art) `shouldReturn` Just (IdentityCharacter you)

  it "clearing The Getaway flips Out Front to Alongside and resolves the collision" do
    tg <- testGameAgainst "60128a" ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      before <- getVillainHitPoints vid
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 2 FromAbility]
      mainSchemeThreatNow `shouldReturn` 0
      selectAny (AttachmentIs Cards.street60129a) `shouldReturn` False
      selectAny (AttachmentIs Cards.street60129b) `shouldReturn` True
      damageOf (player1 tg) `shouldReturn` 1
      getVillainHitPoints vid `shouldReturn` subtractNatural 2 before

  it "Burning Rubber permits a separate decision for each speed counter" do
    tg <- testGameAgainst "60128a" ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      scheme <- selectJust EncounterMainScheme
      resolve =<< L.runQueueT (L.capture $ L.placeCounters scheme 1)
      before <- mainSchemeThreatNow
      resource <- putInHand you "01089"
      _ <- revealNow you Cards.street60132
      choose "pay for the first speed counter" (isLabel "$label.fneSpendResource")
      payWith [resource]
      choose "add threat for the second" (isLabel "$label.fnePlaceThreat")
      pass
      mainSchemeThreatNow `shouldReturn` (before + 1)

  it "Pedestrians exhausts a chosen character and then offers indirect damage when it is no longer ready" do
    tg <- testGameAgainst "60128a" ["spider-man"]
    inGame tg do
      let you = player1 tg
      scheme <- selectJust EncounterMainScheme
      resolve =<< L.runQueueT (L.capture $ L.placeCounters scheme 1)
      _ <- revealNow you Cards.street60133
      choose "exhaust the identity" (isTargetChoice you)
      offers (isTargetChoice you) `shouldReturn` False
      choose "take damage for the second speed counter" (isLabel "$label.fneTakeIndirectDamage")
      pass
      isExhaustedPlayer you `shouldReturn` True
      damageOf you `shouldReturn` 1
