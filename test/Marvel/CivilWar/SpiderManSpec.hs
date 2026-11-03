module Marvel.CivilWar.SpiderManSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (attachmentIdOf,withLeader,leaderHp)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.Modifier
import TestImport

spec :: Spec
spec = describe "Civil War Spider-Man modular" do
  it "Tangled Up attaches to a cooperative hero and replaces a basic attack" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      card <- encounterCard C.cw56181
      resolve [RevealEncounterCard you card]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (IdentityCharacter you)
      vid <- villainIdNow
      before <- leaderHp vid
      choose "attempt the attack" $ \case UseAbility ability -> abilityIndex ability == 301; _ -> False
      leaderHp vid `shouldReturn` before
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing

  it "Tangled Up is discarded when a cooperative alter-ego has no hero to attach it to" do
    tg <- testGame ["spider-man"]
    inGame tg do
      card <- encounterCard C.cw56181
      resolve [RevealEncounterCard (player1 tg) card]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing
      getEncounterDiscard >>= (`shouldSatisfy` elem card)

  it "Tangled Up attaches to the friendly leader across the table and replaces its scheme" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      friendly <- addSecondBoard tg
      card <- encounterCard C.cw56181
      resolve [RevealEncounterCard (player1 tg) card]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter friendly)
      before <- mainSchemeThreat
      resolve [VillainMessage friendly VillainSchemes]
      mainSchemeThreat `shouldReturn` before
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing

  it "Tangled Up replaces a hero's thwart event after its cost is paid" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      L.runQueueT (L.capture $ L.placeThreatOnEach GameRef 5 EncounterMainScheme) >>= resolve
      card <- encounterCard C.cw56181
      resolve [RevealEncounterCard you card]
      event <- putInHand you "01060"
      resource <- putInHand you "01089"
      playWith event [resource]
      mainSchemeThreat `shouldReturn` 5
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing

  it "Spectacular places two threat in cooperative play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      card <- encounterCard C.cw56182
      resolve [RevealEncounterCard (player1 tg) card]
      mainSchemeThreat `shouldReturn` 2

  it "Neighborhood Hero places two threat per player in cooperative play in addition to its base" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      card <- encounterCard C.cw56183
      resolve [RevealEncounterCard (player1 tg) card]
      sideSchemeThreat (SideSchemeId $ unCardId $ ecCardId card) `shouldReturn` 6

  it "Neighborhood Hero moves only the available threat from the opposite board" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      friendly <- addSecondBoard tg
      area <- getAreaOfTarget friendly
      mains <- getMainSchemesIn area
      L.runQueueT (L.capture $ for_ mains $ \mid -> do
        n <- getSchemeThreatAmount $ SchemeMainSchemeId mid
        L.removeSchemeThreat GameRef (SchemeMainSchemeId mid) n
        L.placeThreatOn GameRef mid 1) >>= resolve
      card <- encounterCard C.cw56183
      resolve [RevealEncounterCard (player1 tg) card]
      sideSchemeThreat (SideSchemeId $ unCardId $ ecCardId card) `shouldReturn` 3
      traverse (getSchemeThreatAmount . SchemeMainSchemeId) mains `shouldReturn` replicate (length mains) 0

  it "Spider-Man searches for Tangled Up when no tangled character can be attacked" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      [tangled,_] <- stackEncounterDeck [C.cw56181,M.hydraMercenary]
      _ <- addMinion you C.cw56180
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` [tangled]

  it "Spider-Man attacks a tangled hero and does not search for another copy" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      tangled <- encounterCard C.cw56181
      resolve [RevealEncounterCard you tangled]
      _ <- stackEncounterDeck [C.cw56181,M.hydraMercenary]
      _ <- addMinion you C.cw56180
      damageOf you `shouldReturn` 2
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` []

  it "Spider-Man attacks a tangled friendly leader across the table" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      friendly <- addSecondBoard tg
      tangled <- encounterCard C.cw56181
      resolve [RevealEncounterCard (player1 tg) tangled]
      before <- leaderHp friendly
      _ <- addMinion (player1 tg) C.cw56180
      leaderHp friendly `shouldReturn` (before - 2)

  it "a stun replaces the hero's attack before Tangled Up is spent" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      card <- encounterCard C.cw56181
      resolve [RevealEncounterCard you card,GiveStatus (IdentityRef you) StunnedStatus]
      choose "attempt the stunned attack" $ \case UseAbility ability -> abilityIndex ability == 301; _ -> False
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (IdentityCharacter you)
      getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 0

  it "Tangled Up replaces a basic thwart" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      L.runQueueT (L.capture $ L.placeThreatOnEach GameRef 4 EncounterMainScheme) >>= resolve
      card <- encounterCard C.cw56181
      resolve [RevealEncounterCard you card]
      choose "attempt the thwart" $ \case UseAbility ability -> abilityIndex ability == 300; _ -> False
      mainSchemeThreat `shouldReturn` 4
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing

  it "Tangled Up replaces a paid attack event without damaging the enemy" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      vid <- villainIdNow
      before <- leaderHp vid
      card <- encounterCard C.cw56181
      resolve [RevealEncounterCard you card]
      event <- putInHand you "01005"
      first <- putInHand you "01088"
      second <- putInHand you "01090"
      playWith event [first,second]
      leaderHp vid `shouldReturn` before
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing

  it "Tangled Up's boost is discarded normally if the Spider-Man minion is absent" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      [card,_] <- stackEncounterDeck [C.cw56181,M.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing
      getEncounterDiscard >>= (`shouldSatisfy` elem card)

  it "Tangled Up's boost reveals and attaches it when the Spider-Man minion is present" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      _ <- addMinion you C.cw56180
      changeForm you
      [card,_] <- stackEncounterDeck [C.cw56181,M.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (IdentityCharacter you)
      getEncounterDiscard >>= (`shouldSatisfy` notElem card)

  it "Neighborhood Hero uses otherwise when the competitive leader has no threat to move" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      friendly <- addSecondBoard tg
      area <- getAreaOfTarget friendly
      mains <- getMainSchemesIn area
      L.runQueueT (L.capture $ for_ mains $ \mid -> do
        n <- getSchemeThreatAmount $ SchemeMainSchemeId mid
        L.removeSchemeThreat GameRef (SchemeMainSchemeId mid) n) >>= resolve
      card <- encounterCard C.cw56183
      resolve [RevealEncounterCard (player1 tg) card]
      sideSchemeThreat (SideSchemeId $ unCardId $ ecCardId card) `shouldReturn` 4

  it "Tangled Up replaces a leader's cross-board attack before a boost is dealt" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      own <- withLeader "57040"
      friendly <- addSecondBoard tg
      card <- encounterCard C.cw56181
      resolve [RevealEncounterCard (player1 tg) card]
      before <- leaderHp own
      [next,_] <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier]
      L.runQueueT (L.capture $ L.villainAttacksCharacter friendly (player1 tg) own) >>= resolve
      leaderHp own `shouldReturn` before
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing
      headMay <$> getEncounterDeck `shouldReturn` Just next

  it "only one copy of Tangled Up is spent replacing a hero's power" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      first <- encounterCard C.cw56181
      second <- encounterCard C.cw56181
      resolve [RevealEncounterCard you first,RevealEncounterCard you second]
      choose "attempt the attack" $ \case UseAbility ability -> abilityIndex ability == 301; _ -> False
      choose "discard the first Tangled Up" (isTargetChoice $ attachmentIdOf first)
      getAttachedCharacter (attachmentIdOf first) `shouldReturn` Nothing
      getAttachedCharacter (attachmentIdOf second) `shouldReturn` Just (IdentityCharacter you)

  it "Spectacular places local threat and removes threat from the friendly leader's main scheme" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      own <- villainIdNow
      ownArea <- getAreaOfTarget own
      local <- getMainSchemesIn ownArea
      friendly <- addSecondBoard tg
      area <- getAreaOfTarget friendly
      other <- getMainSchemesIn area
      L.runQueueT (L.capture $ traverse_ (\mid -> L.placeThreatOn GameRef mid 4) other) >>= resolve
      before <- traverse (getSchemeThreatAmount . SchemeMainSchemeId) other
      card <- encounterCard C.cw56182
      resolve [RevealEncounterCard you card]
      sum <$> traverse (getSchemeThreatAmount . SchemeMainSchemeId) local `shouldReturn` 2
      traverse (getSchemeThreatAmount . SchemeMainSchemeId) other `shouldReturn` map (subtract 2) before

  it "Tangled Up replaces Grapnel Launcher's basic thwart after the launcher is discarded as its cost" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      L.runQueueT (L.capture $ L.placeThreatOnEach GameRef 5 EncounterMainScheme) >>= resolve
      launcher <- putInHand you "61023"
      L.runQueueT (L.capture $ L.putIntoPlay you launcher) >>= resolve
      tangled <- encounterCard C.cw56181
      resolve [RevealEncounterCard you tangled]
      choose "use Grapnel Launcher" (isAbilityOf $ upgradeIdOf launcher)
      mainSchemeThreat `shouldReturn` 5
      getAttachedCharacter (attachmentIdOf tangled) `shouldReturn` Nothing
      discardOf you >>= (`shouldSatisfy` elem launcher)

  it "Spider-Man still searches when he cannot attack an otherwise valid tangled target" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      tangled <- encounterCard C.cw56181
      resolve [RevealEncounterCard you tangled]
      L.runQueueT (L.capture $ L.lastingModifiers GameRef (EnemyEntity $ EnemyIs C.cw56180) [CannotActivate] False) >>= resolve
      [next,_] <- stackEncounterDeck [C.cw56181,M.hydraMercenary]
      _ <- addMinion you C.cw56180
      damageOf you `shouldReturn` 0
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` [next]
