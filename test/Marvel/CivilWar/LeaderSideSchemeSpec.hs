module Marvel.CivilWar.LeaderSideSchemeSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader,attachmentIdOf,leaderHp)
import Marvel.CivilWar.LeaderSideSchemes (leaderSideSchemeDefinitions)
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.Query
import TestImport

playCovertOps :: IdentityId -> SideSchemeId -> GameAppT ()
playCovertOps you sid = do
  card <- putInHand you "08003"
  resources <- traverse (putInHand you) ["01089","01093"]
  playWith card resources
  choose "remove threat from the leader's side scheme" (isTargetChoice sid)

spec :: Spec
spec = describe "Leader side schemes" do
  for_ leaderSideSchemeDefinitions $ \def ->
    it ("starts " <> show (toCardCode def) <> " with three threat per player") do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        card <- encounterCard def
        resolve [RevealEncounterCard (player1 tg) card]
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 6

  it "Stark Tower lets the enemy team choose an Iron Man attachment from the discard pile" do
    tg <- testGame ["black-widow"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "56059"
      (_, [helmet]) <- setEncounterDeck [C.cw56065] [C.cw56067]
      tower <- encounterCard C.cw56072
      resolve [RevealEncounterCard you tower]
      playCovertOps you (sideSchemeOf tower)
      choose "choose Mark V Helmet from the discard pile" $ \case CardLabel code _ -> code == "56067"; _ -> False
      pass
      getAttachedCharacter (attachmentIdOf helmet) `shouldReturn` Just (VillainCharacter vid)
      getEncounterDiscard >>= (`shouldSatisfy` notElem helmet)
      villainMatches VillainWithToughStatus vid `shouldReturn` True

  it "Alpha Flight Station adds three energy without initiating Energy Channel's attack" do
    tg <- testGame ["black-widow"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "56092"
      energy <- encounterCard C.cw56098
      station <- encounterCard C.cw56104
      resolve [RevealEncounterCard you energy,PlaceCounters (AttachmentRef $ attachmentIdOf energy) 2,RevealEncounterCard you station]
      playCovertOps you (sideSchemeOf station)
      pass
      getCountersOn (attachmentIdOf energy) `shouldReturn` 5
      leaderHp vid `shouldReturn` 14
      damageOf you `shouldReturn` 0

  for_ [False,True] $ \hero ->
    it ("Fearless Determination returns the Shield and activates against the defeater: hero " <> show hero) do
      tg <- testGame ["black-widow"]
      inGame tg do
        let you = player1 tg
        when hero $ changeForm you
        vid <- withLeader "56137"
        shield <- encounterCard C.cw56143
        scheme <- encounterCard C.cw56149
        resolve [RevealEncounterCard you shield,RevealEncounterCard you scheme]
        messages <- L.runQueueT $ L.capture $ L.attachToCharacter (attachmentIdOf shield) (IdentityCharacter you)
        resolve messages
        _ <- stackEncounterDeck [M.hydraMercenary]
        before <- mainSchemeThreatNow
        playCovertOps you (sideSchemeOf scheme)
        choose "Captain America gains tough after reclaiming the Shield" (isTargetChoice vid)
        pass
        getAttachedCharacter (attachmentIdOf shield) `shouldReturn` Just (VillainCharacter vid)
        if hero then damageOf you `shouldReturn` 4 else mainSchemeThreatNow `shouldReturn` (before + 3)

  for_ [0,1] $ \counters ->
    it ("Self-propelled Glide reduces attack damage by the actual skill counters: " <> show counters) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        changeForm you
        vid <- withLeader "56168"
        finesse <- encounterCard C.cw56174
        glide <- encounterCard C.cw56179
        resolve [RevealEncounterCard you finesse,PlaceCounters (AttachmentRef $ attachmentIdOf finesse) counters,RevealEncounterCard you glide]
        before <- leaderHp vid
        choose "make a basic attack" $ \case UseAbility ab -> abilityIndex ab == 301; _ -> False
        choose "attack Spider-Woman" (isTargetChoice vid)
        when (counters > 0) $ choose "resolve Glide's prevention" (isAbilityOf $ sideSchemeOf glide)
        pass
        leaderHp vid `shouldReturn` (before - 2 + fromIntegral counters)
        getCountersOn (attachmentIdOf finesse) `shouldReturn` counters

  it "Self-propelled Glide finds Finesse when the permanent attachment enters later" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      vid <- withLeader "56168"
      glide <- encounterCard C.cw56179
      finesse <- encounterCard C.cw56174
      resolve [RevealEncounterCard you glide,RevealEncounterCard you finesse,PlaceCounters (AttachmentRef $ attachmentIdOf finesse) 1]
      before <- leaderHp vid
      choose "make a basic attack" $ \case UseAbility ab -> abilityIndex ab == 301; _ -> False
      choose "attack Spider-Woman" (isTargetChoice vid)
      choose "resolve Glide's prevention" (isAbilityOf $ sideSchemeOf glide)
      pass
      leaderHp vid `shouldReturn` (before - 1)

  it "Legal Practice prevents heroes and allies from thwarting it" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- withLeader "57001"
      legal <- encounterCard C.cw57012
      resolve [RevealEncounterCard you legal]
      cat <- putIntoPlay you "01067"
      offers (\case UseAbility ab -> abilitySource ab == IdentityRef you && abilityIndex ab == 300; _ -> False) `shouldReturn` False
      offers (\case UseAbility ab -> abilitySource ab == AllyRef (allyIdOf cat) && abilityIndex ab == 300; _ -> False) `shouldReturn` False
      sideSchemeThreat (sideSchemeOf legal) `shouldReturn` 3

  it "Legal Practice lets the player exhaust their identity after changing to alter-ego" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- withLeader "57001"
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraMercenary]
      endTurn
      pass
      legal <- encounterCard C.cw57012
      resolve [RevealEncounterCard you legal]
      changeForm you
      choose "resolve Legal Practice" (isAbilityOf $ sideSchemeOf legal)
      chooseIf (isTargetChoice you)
      pass
      isExhaustedNow you `shouldReturn` True
      selectAny (SideSchemeWithId $ sideSchemeOf legal) `shouldReturn` False

  it "Legal Practice can also be defeated by a thwart event used in alter-ego form" do
    tg <- testGame ["black-widow"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "57001"
      legal <- encounterCard C.cw57012
      resolve [RevealEncounterCard you legal]
      playCovertOps you (sideSchemeOf legal)
      pass
      selectAny (SideSchemeWithId $ sideSchemeOf legal) `shouldReturn` False

  it "Just Passing Through confuses the player who defeats it" do
    tg <- testGame ["black-widow"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "57040"
      passing <- encounterCard C.cw57054
      resolve [RevealEncounterCard you passing]
      playCovertOps you (sideSchemeOf passing)
      pass
      isConfused you `shouldReturn` True
      isStunned you `shouldReturn` False

  it "Just Passing Through stuns instead when it cannot add confusion" do
    tg <- testGame ["black-widow"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "57040"
      passing <- encounterCard C.cw57054
      resolve [RevealEncounterCard you passing,GiveStatus (IdentityRef you) ConfusedStatus]
      teams <- traverse (const $ putIntoPlay you "01064") [1..3 :: Int]
      for_ teams $ \team -> do
        choose "use a Surveillance Team" (isAbilityOf $ supportIdOf team)
        choose "remove threat without thwarting" (isTargetChoice $ sideSchemeOf passing)
        pass
      isConfused you `shouldReturn` True
      isStunned you `shouldReturn` True
