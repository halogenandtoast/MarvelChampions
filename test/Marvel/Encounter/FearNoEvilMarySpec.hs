module Marvel.Encounter.FearNoEvilMarySpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Types qualified as Attachment
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Query
import TestImport

walkerFixture :: IdentityId -> GameAppT (VillainId,SideSchemeId)
walkerFixture you = do
  vid <- villainIdNow
  resolve [ReplaceVillain vid "60110a" Advanced]
  _ <- revealNow you Cards.street60112
  pass
  _ <- revealNow you Cards.street60113a
  pass
  sid <- selectJust $ SideSchemeIs Cards.street60113a
  resolve =<< L.runQueueT (L.capture $ L.removeSchemeThreat GameRef (SchemeSideSchemeId sid) 8)
  pure (vid,sid)

spec :: Spec
spec = describe "Fear No Evil Mary behaviors" do
  for_ [(Cards.street60115,2,0),(Cards.street60118,0,1)] $ \(def,threat,damage) ->
    it ("revealing " <> show (toCardCode def) <> " breaks trust before its printed activation, even in alter-ego form") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        (_,sid) <- walkerFixture you
        _ <- setEncounterDeck (replicate 4 Cards.street60188) []
        card <- encounterCard def
        resolve [RevealEncounterCard you card]
        selectAny (SideSchemeWithId sid) `shouldReturn` True
        selectAny (AttachmentWithId $ AttachmentId $ unSideSchemeId sid) `shouldReturn` False
        getSchemeThreatAmount (SchemeSideSchemeId sid) `shouldReturn` 8
        mainSchemeThreatNow `shouldReturn` threat
        damageOf you `shouldReturn` damage
        length . playerIdentityEncounterCards . toAttrs <$> getPlayer you `shouldReturn` 0

  it "Scavenged Bulwark gives Mary tough and resolves one surge card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- villainIdNow
      resolve [ReplaceVillain vid "60110a" Advanced]
      _ <- setEncounterDeck [Cards.street60085,Cards.street60188,Cards.street60188,Cards.street60188] []
      card <- encounterCard Cards.street60119
      resolve [RevealEncounterCard you card]
      getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 1
      length <$> getEncounterDeck `shouldReturn` 3

  it "expert Typhoid Mary flips with her existing damage and does not resolve Bloody Mary's When Revealed" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      resolve [ReplaceVillain vid "60111a" Advanced]
      getVillainHitPoints vid `shouldReturn` 13
      getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 1
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 1 FromAbility]
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 3 FromAbility,VillainMessage vid FlipVillain]
      toCardCode . getCardDef <$> getVillain `shouldReturn` "60111b"
      getVillainHitPoints vid `shouldReturn` 10
      damageOf (player1 tg) `shouldReturn` 0
      resolve [VillainMessage vid FlipVillain]
      getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 0

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
