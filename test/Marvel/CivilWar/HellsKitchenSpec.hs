module Marvel.CivilWar.HellsKitchenSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (attachmentIdOf,withLeader,leaderHp)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Minion.Cards qualified as M
import Marvel.Modifier
import Marvel.Query (selectAny)
import Marvel.Message.Lifted qualified as L
import TestImport hiding (ExhaustedIdentity,ExhaustedAlly)

spec :: Spec
spec = describe "Civil War Hell's Kitchen" do
  it "Daredevil exhausts the revealing player's identity in cooperative play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- addMinion you C.cw56189
      selectAny (IdentityWithId you <> ExhaustedIdentity) `shouldReturn` True

  it "Daredevil's boost exhausts a character without entering play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      _ <- stackEncounterDeck [C.cw56189,M.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      selectAny (IdentityWithId you <> ExhaustedIdentity) `shouldReturn` True
      selectAny (MinionIs C.cw56189) `shouldReturn` False

  it "Daredevil finishes without a question if all controlled characters are exhausted" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      L.runQueueT (L.capture $ L.exhaust you) >>= resolve
      _ <- addMinion you C.cw56189
      selectAny (IdentityWithId you <> ExhaustedIdentity) `shouldReturn` True

  it "Iron Fist schemes against the revealing alter-ego outside competitive play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- addMinion (player1 tg) C.cw56190
      mainSchemeThreat `shouldReturn` 1

  it "Iron Fist attacks the friendly leader across the table" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      friendly <- addSecondBoard tg
      before <- leaderHp friendly
      _ <- addMinion (player1 tg) C.cw56190
      leaderHp friendly `shouldReturn` (before - 3)
      damageOf (player1 tg) `shouldReturn` 0

  it "Resistance Fighter gives its minion four hit points and stalwart" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you M.hydraSoldier
      card <- encounterCard C.cw56191
      resolve [RevealEncounterCard you card]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (MinionCharacter mid)
      modifiers <- getModifiers (MinionRef mid)
      modifiers `shouldSatisfy` elem (HitPointModifier 4)
      getEnemyKeywords (EnemyMinionId mid) >>= (`shouldSatisfy` elem K.Stalwart)
      resolve [GiveStatus (MinionRef mid) StunnedStatus]
      getCharacterStatusCount (MinionCharacter mid) StunnedStatus `shouldReturn` 0

  it "Resistance Fighter surges when there are no minions" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      [next,_] <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier]
      card <- encounterCard C.cw56191
      resolve [RevealEncounterCard you card]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` [next]

  it "Resistance Fighter moves from boost to attachment on an engaged minion" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      mid <- addMinion you M.hydraSoldier
      other <- addMinion (player2 tg) M.hydraSoldier
      [card,_] <- stackEncounterDeck [C.cw56191,M.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (MinionCharacter mid)
      getEnemyKeywords (EnemyMinionId other) >>= (`shouldSatisfy` notElem K.Stalwart)
      getEncounterDiscard >>= (`shouldSatisfy` notElem card)

  it "Defend Hell's Kitchen protects its threat only while a Defender minion is present" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you C.cw56189
      card <- encounterCard C.cw56193
      resolve [RevealEncounterCard you card]
      let sid = SideSchemeId $ unCardId $ ecCardId card
      sideSchemeThreat sid `shouldReturn` 5
      getModifiers (SideSchemeRef sid) >>= (`shouldSatisfy` elem ThreatCannotBeRemoved)
      resolve [RemoveFromPlay $ MinionRef mid]
      getModifiers (SideSchemeRef sid) >>= (`shouldSatisfy` notElem ThreatCannotBeRemoved)

  it "Daredevil gives the opposing team the exhaustion choice and excludes other players' characters" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- addSecondBoard tg
      ally <- putInHand you "01050"
      L.runQueueT (L.capture $ L.putIntoPlay you ally) >>= resolve
      card <- encounterCard C.cw56189
      start [RevealEncounterCard you card]
      fmap fst <$> openQuestion `shouldReturn` Just (player2 tg)
      choose "exhaust the ally" (isTargetChoice $ AllyCharacter $ allyIdOf ally)
      pass
      selectAny (IdentityWithId you <> ExhaustedIdentity) `shouldReturn` False
      selectAny (AllyWithId (allyIdOf ally) <> ExhaustedAlly) `shouldReturn` True

  it "Iron Fist's boost schedules a leader attack without drawing another boost" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      own <- withLeader "57040"
      friendly <- addSecondBoard tg
      before <- leaderHp friendly
      [_,next] <- stackEncounterDeck [C.cw56190,M.hydraMercenary]
      resolve [VillainMessage own VillainSchemes]
      leaderHp friendly `shouldReturn` (before - 1)
      headMay <$> getEncounterDeck `shouldReturn` Just next

  it "Iron Fist's boost has no friendly leader to attack in cooperative play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      own <- withLeader "57040"
      [_,next] <- stackEncounterDeck [C.cw56190,M.hydraMercenary]
      resolve [VillainMessage own VillainSchemes]
      damageOf (player1 tg) `shouldReturn` 0
      headMay <$> getEncounterDeck `shouldReturn` Just next

  it "Resistance Fighter's reveal gives the enemy team a choice among minions on the revealing board" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- addSecondBoard tg
      first <- addMinion you M.hydraSoldier
      second <- addMinion you M.hydraMercenary
      card <- encounterCard C.cw56191
      start [RevealEncounterCard you card]
      fmap fst <$> openQuestion `shouldReturn` Just (player2 tg)
      choose "attach to the mercenary" (isTargetChoice $ EnemyMinionId second)
      pass
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (MinionCharacter second)
      getEnemyKeywords (EnemyMinionId first) >>= (`shouldSatisfy` notElem K.Stalwart)

  it "Resistance Fighter's boost discards normally if only another player's minion is available" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      own <- withLeader "57040"
      _ <- addMinion (player2 tg) M.hydraSoldier
      [card,_] <- stackEncounterDeck [C.cw56191,M.hydraMercenary]
      resolve [VillainMessage own VillainSchemes]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing
      getEncounterDiscard >>= (`shouldSatisfy` elem card)
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` []

  it "Defend Hell's Kitchen ignores Defenders on the other team's board" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- addSecondBoard tg
      _ <- addMinion (player2 tg) C.cw56189
      card <- encounterCard C.cw56193
      resolve [RevealEncounterCard you card]
      let sid = SideSchemeId $ unCardId $ ecCardId card
      sideSchemeThreat sid `shouldReturn` 4
      getModifiers (SideSchemeRef sid) >>= (`shouldSatisfy` notElem ThreatCannotBeRemoved)
