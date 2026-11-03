module Marvel.CivilWar.CloakAndDaggerSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (attachmentIdOf,withLeader,leaderHp)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Damage
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.Query (selectAny,selectJust)
import TestImport

spec :: Spec
spec = describe "Civil War Cloak and Dagger" do
  it "Cloak places two threat in cooperative play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- addMinion (player1 tg) C.cw56194
      mainSchemeThreat `shouldReturn` 2

  it "Cloak places local threat and removes threat from the friendly leader's board" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      own <- villainIdNow
      ownArea <- getAreaOfTarget own
      local <- getMainSchemesIn ownArea
      friendly <- addSecondBoard tg
      area <- getAreaOfTarget friendly
      schemes <- getMainSchemesIn area
      L.runQueueT (L.capture $ traverse_ (\sid -> L.placeThreatOn GameRef sid 4) schemes) >>= resolve
      before <- traverse (getSchemeThreatAmount . SchemeMainSchemeId) schemes
      _ <- addMinion (player1 tg) C.cw56194
      sum <$> traverse (getSchemeThreatAmount . SchemeMainSchemeId) local `shouldReturn` 2
      traverse (getSchemeThreatAmount . SchemeMainSchemeId) schemes `shouldReturn` map (subtract 2) before

  it "Cloak's boost places one threat when Dagger is absent" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      _ <- stackEncounterDeck [C.cw56194,M.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      mainSchemeThreat `shouldReturn` 2
      selectAny (MinionIs C.cw56194) `shouldReturn` False

  it "Cloak's boost reveals the physical card and resolves its When Revealed when Dagger is present" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      _ <- addMinion you C.cw56195
      [card,_] <- stackEncounterDeck [C.cw56194,M.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      mainSchemeThreat `shouldReturn` 3
      selectAny (MinionIs C.cw56194) `shouldReturn` True
      getEncounterDiscard >>= (`shouldSatisfy` notElem card)

  it "Dagger heals the enemy leader and damages a controlled character" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 4 FromAbility]
      before <- leaderHp vid
      _ <- addMinion you C.cw56195
      leaderHp vid `shouldReturn` (before + 2)
      damageOf you `shouldReturn` 2

  it "Dagger's boost deals one damage without entering play if Cloak is absent" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      _ <- stackEncounterDeck [C.cw56195,M.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      damageOf (player1 tg) `shouldReturn` 1
      selectAny (MinionIs C.cw56195) `shouldReturn` False

  it "Dagger's boost reveals Dagger and deals two damage when Cloak is present" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      _ <- addMinion you C.cw56194
      [card,_] <- stackEncounterDeck [C.cw56195,M.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      damageOf you `shouldReturn` 2
      selectAny (MinionIs C.cw56195) `shouldReturn` True
      getEncounterDiscard >>= (`shouldSatisfy` notElem card)

  it "Darkforce attaches and gives tough, then is spent after a scheme places threat" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      card <- encounterCard C.cw56196
      resolve [RevealEncounterCard you card]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter vid)
      getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 1
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier]
      resolve [VillainMessage vid VillainSchemes]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing

  it "Darkforce survives a scheme replaced by confusion" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      card <- encounterCard C.cw56196
      resolve [RevealEncounterCard you card,GiveStatus (VillainRef vid) ConfusedStatus]
      resolve [VillainMessage vid VillainSchemes]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter vid)

  it "Lightforce heals the enemy leader and deals three indirect damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 4 FromAbility]
      before <- leaderHp vid
      card <- encounterCard C.cw56197
      resolve [RevealEncounterCard you card]
      leaderHp vid `shouldReturn` (before + 3)
      damageOf you `shouldReturn` 3

  it "Cloak and Dagger uses hinder and heals and toughens the enemy leader when defeated" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 4 FromAbility]
      before <- leaderHp vid
      card <- encounterCard C.cw56198
      resolve [RevealEncounterCard you card]
      let sid = SideSchemeId $ unCardId $ ecCardId card
      sideSchemeThreat sid `shouldReturn` 5
      L.runQueueT (L.capture $ L.removeSchemeThreat you (SchemeSideSchemeId sid) 5) >>= resolve
      leaderHp vid `shouldReturn` (before + 3)
      getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 1

  it "Darkforce removes the actual scheme amount from the opposite board and is discarded" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      own <- withLeader "57040"
      friendly <- addSecondBoard tg
      area <- getAreaOfTarget friendly
      schemes <- getMainSchemesIn area
      L.runQueueT (L.capture $ traverse_ (\sid -> L.placeThreatOn GameRef sid 4) schemes) >>= resolve
      before <- traverse (getSchemeThreatAmount . SchemeMainSchemeId) schemes
      card <- encounterCard C.cw56196
      resolve [RevealEncounterCard you card]
      _ <- stackEncounterDeck [M.hydraSoldier,M.hydraMercenary]
      resolve [VillainMessage own VillainSchemes]
      traverse (getSchemeThreatAmount . SchemeMainSchemeId) schemes `shouldReturn` map (subtract 2) before
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing
      getEncounterDiscard >>= (`shouldSatisfy` elem card)

  it "Darkforce stays attached if the leader schemes for zero" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      own <- withLeader "57040"
      card <- encounterCard C.cw56196
      resolve [RevealEncounterCard you card]
      L.runQueueT (L.capture $ L.lastingModifiers GameRef (SourceEntity $ VillainRef own) [SchemePenalty 1] False) >>= resolve
      _ <- stackEncounterDeck [C.cw56194,M.hydraMercenary]
      resolve [VillainMessage own VillainSchemes]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter own)

  it "a partner on the other board does not make Cloak's boost reveal him" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      own <- withLeader "57040"
      _ <- addSecondBoard tg
      _ <- addMinion (player2 tg) C.cw56195
      _ <- stackEncounterDeck [C.cw56194,M.hydraMercenary]
      resolve [VillainMessage own VillainSchemes]
      asYou (player1 tg) (selectAny $ MinionIs C.cw56194) `shouldReturn` False

  it "Dagger lets the revealing player assign her damage to a controlled ally" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      ally <- putInHand you "01050"
      L.runQueueT (L.capture $ L.putIntoPlay you ally) >>= resolve
      card <- encounterCard C.cw56195
      start [RevealEncounterCard you card]
      choose "damage Hulk" (isTargetChoice $ AllyCharacter $ allyIdOf ally)
      pass
      damageOf you `shouldReturn` 0
      damageOf (player2 tg) `shouldReturn` 0
      getsGame (fmap (allyDamage . toAttrs) . lookup (allyIdOf ally) . gameAllies) `shouldReturn` Just 2

  it "the side scheme's defeat heals its own board's leader even when an opponent removes its threat" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      own <- withLeader "57040"
      other <- addSecondBoard tg
      resolve [VillainMessage own $ VillainDamaged GameRef $ toDamage 4 FromAbility,VillainMessage other $ VillainDamaged GameRef $ toDamage 4 FromAbility]
      before <- leaderHp own
      otherBefore <- leaderHp other
      card <- encounterCard C.cw56198
      resolve [RevealEncounterCard you card]
      let sid = SideSchemeId $ unCardId $ ecCardId card
      n <- sideSchemeThreat sid
      L.runQueueT (L.capture $ L.removeSchemeThreat (player2 tg) (SchemeSideSchemeId sid) n) >>= resolve
      leaderHp own `shouldReturn` (before + 3)
      leaderHp other `shouldReturn` otherBefore
      getCharacterStatusCount (VillainCharacter own) ToughStatus `shouldReturn` 1
      getCharacterStatusCount (VillainCharacter other) ToughStatus `shouldReturn` 0

  it "Darkforce is not spent when Wrecker schemes onto his signature side scheme" do
    tg <- testGameAgainst "07001" ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- selectJust ActiveVillain
      sid <- fromMaybe (error "missing signature scheme") <$> L.signatureSchemeOf vid
      before <- sideSchemeThreat sid
      card <- encounterCard C.cw56196
      resolve [RevealEncounterCard you card]
      _ <- stackEncounterDeck [M.hydraSoldier,M.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      after <- sideSchemeThreat sid
      after `shouldSatisfy` (> before)
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter vid)
