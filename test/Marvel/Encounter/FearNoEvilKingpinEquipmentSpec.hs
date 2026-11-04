module Marvel.Encounter.FearNoEvilKingpinEquipmentSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Types qualified as Attachment
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Villain.Types qualified as Villain
import TestImport

spec :: Spec
spec = describe "Fear No Evil Kingpin equipment" do
  it "Bear Hug replaces three basic attacks with grip removal and then discards itself" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      vid <- villainIdNow
      resolve [ReplaceVillain vid "60159b" Advanced]
      card <- revealNow you Cards.street60164
      pass
      let aid = AttachmentId $ unCardId $ ecCardId card
      getModifiers you >>= (liftIO . (`shouldContain` [CannotChangeToAlterEgoForm]))
      for_ [2,1,0 :: Natural] $ \remaining -> do
        choose "attempt a basic attack" (isHeroAttack you)
        pass
        getVillainHitPoints vid `shouldReturn` 25
        when (remaining > 0) do
          getCountersOn aid `shouldReturn` remaining
          resolve [IdentityMessage you ReadiedIdentity]
      selectAny (AttachmentWithId aid) `shouldReturn` False
      getModifiers you >>= (liftIO . (`shouldNotContain` [CannotChangeToAlterEgoForm]))

  it "Mountain of Muscle absorbs a whole damage packet and discards at eight damage without overflow" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      vid <- villainIdNow
      resolve [ReplaceVillain vid "60159b" Advanced]
      card <- revealNow (player1 tg) Cards.street60167
      pass
      let aid = AttachmentId $ unCardId $ ecCardId card
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 5 FromAbility]
      getVillainHitPoints vid `shouldReturn` 25
      getsGame (Attachment.attachmentDamage . toAttrs . (HashMap.! aid) . gameAttachments) `shouldReturn` 5
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 7 FromAbility]
      getVillainHitPoints vid `shouldReturn` 25
      selectAny (AttachmentWithId aid) `shouldReturn` False

  it "James Wesley supplies a facedown boost, accepts a discarded Thwart event, and makes Kingpin scheme" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      vid <- villainIdNow
      resolve [ReplaceVillain vid "60159b" Advanced]
      _ <- setEncounterDeck (replicate 6 Cards.street60188) []
      card <- revealNow you Cards.street60165
      pass
      length . Villain.villainBoostCards . toAttrs <$> getVillain `shouldReturn` 1
      event <- putInHand you "01060"
      let aid = AttachmentId $ unCardId $ ecCardId card
      choose "discard Wesley" (isAbilityOf aid)
      chooseIf (isTargetChoice event)
      chooseIf (isAbilityOf vid)
      pass
      selectAny (AttachmentWithId aid) `shouldReturn` False
      mainSchemeThreatNow `shouldReturn` 2
      discardOf you >>= \cards -> liftIO $ map pcCardId cards `shouldContain` [pcCardId event]

  it "Vanessa confuses the revealing player and her discarded Attack-event cost causes an immediate attack" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      vid <- villainIdNow
      resolve [ReplaceVillain vid "60159b" Advanced]
      _ <- setEncounterDeck (replicate 6 Cards.street60188) []
      card <- revealNow you Cards.street60168
      pass
      getCharacterStatusCount (IdentityCharacter you) ConfusedStatus `shouldReturn` 1
      event <- putInHand you "01013"
      let aid = AttachmentId $ unCardId $ ecCardId card
      choose "discard Vanessa" (isAbilityOf aid)
      chooseIf (isTargetChoice event)
      chooseIf (isAbilityOf vid)
      pass
      selectAny (AttachmentWithId aid) `shouldReturn` False
      damageOf you `shouldReturn` 2

  it "Kingpin's Cane stays when its damaged hero was already stunned and discards when it newly stuns the hero" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      vid <- villainIdNow
      resolve [ReplaceVillain vid "60159b" Advanced]
      card <- revealNow you Cards.street60166
      pass
      let aid = AttachmentId $ unCardId $ ecCardId card
      _ <- setEncounterDeck (replicate 8 Cards.street60188) []
      resolve [VillainMessage vid $ VillainAttacks you]
      selectAny (AttachmentWithId aid) `shouldReturn` True
      choose "spend the stun on a basic attack" (isHeroAttack you)
      pass
      getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 0
      resolve [VillainMessage vid $ VillainAttacks you]
      selectAny (AttachmentWithId aid) `shouldReturn` False
      damageOf you `shouldReturn` 6
