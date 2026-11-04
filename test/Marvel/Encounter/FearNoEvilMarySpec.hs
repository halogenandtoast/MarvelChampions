module Marvel.Encounter.FearNoEvilMarySpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Types qualified as Attachment
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Query
import TestImport

spec :: Spec
spec = describe "Fear No Evil Mary behaviors" do
  it "flipping Typhoid Mary to Bloody Mary preserves damage and status without revealing her" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      resolve [ReplaceVillain vid "60110a" Advanced]
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 3 FromAbility,GiveStatus (VillainRef vid) ConfusedStatus]
      resolve [VillainMessage vid FlipVillain]
      toCardCode . getCardDef <$> getVillain `shouldReturn` "60110b"
      getVillainHitPoints vid `shouldReturn` 7
      getCharacterStatusCount (VillainCharacter vid) ConfusedStatus `shouldReturn` 1
      damageOf (player1 tg) `shouldReturn` 0

  it "revealing expert Bloody Mary damages every identity once" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      vid <- villainIdNow
      resolve [ReplaceVillain vid "60111b" Advanced]
      damageOf (player1 tg) `shouldReturn` 1
      damageOf (player2 tg) `shouldReturn` 1
      getVillainHitPoints vid `shouldReturn` 26

  it "Telekinesis increases SCH and is discarded by paying two mental resources" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      vid <- villainIdNow
      resolve [ReplaceVillain vid "60110a" Advanced]
      card <- revealNow you Cards.street60114
      pass
      let aid = AttachmentId $ unCardId $ ecCardId card
      genius <- putInHand you "01089"
      choose "discard Telekinesis" (isAbilityOf aid)
      payWith [genius]
      pass
      selectAny (AttachmentWithId aid) `shouldReturn` False
      getEncounterDiscard >>= \cards -> liftIO $ map ecCardId cards `shouldContain` [ecCardId card]

  it "Assembled Armor absorbs the entire hit that crosses its threshold, with no excess damage to Mary" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      resolve [ReplaceVillain vid "60110a" Advanced]
      card <- revealNow (player1 tg) Cards.street60117
      pass
      let aid = AttachmentId $ unCardId $ ecCardId card
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 3 FromAbility]
      getVillainHitPoints vid `shouldReturn` 10
      getsGame (Attachment.attachmentDamage . toAttrs . (HashMap.! aid) . gameAttachments) `shouldReturn` 3
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 4 FromAbility]
      getVillainHitPoints vid `shouldReturn` 10
      selectAny (AttachmentWithId aid) `shouldReturn` False

  it "Psi Nova damages only the revealing player's characters and stuns that identity" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- putIntoPlay you "01050"
      _ <- putIntoPlay (player2 tg) "01002"
      _ <- revealNow you Cards.street60116
      pass
      damageOf you `shouldReturn` 1
      allyDamage <$> allyIn "01050" `shouldReturn` 1
      damageOf (player2 tg) `shouldReturn` 0
      allyDamage <$> allyIn "01002" `shouldReturn` 0
      getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 1

  it "Hand Lookout enters play tough and guards its villain" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street60120
      minionMatches (MinionWithKeyword Guard) mid `shouldReturn` True
      getCharacterStatusCount (MinionCharacter mid) ToughStatus `shouldReturn` 1
