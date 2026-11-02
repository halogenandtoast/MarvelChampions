module Marvel.CivilWar.LeaderTreacherySpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader,attachmentIdOf,leaderHp)
import Marvel.Message.Lifted qualified as L
import Marvel.Matchers
import Marvel.Minion.Cards qualified as M
import Marvel.Query
import Marvel.SideScheme.Cards qualified as S
import TestImport

spec :: Spec
spec = describe "Leader treacheries" do
  it "counts every printed energy and wild resource discarded by Repulsor Blast" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- withLeader "56059"
      _ <- stackDeck you ["01088","01093","01089","01055"]
      blast <- encounterCard C.cw56070
      resolve [RevealEncounterCard you blast]
      damageOf you `shouldReturn` 4

  it "discards a controlled upgrade or support for alter-ego Repulsor Blast" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "56059"
      mansion <- putIntoPlay you "01091"
      blast <- encounterCard C.cw56070
      start [RevealEncounterCard you blast]
      choose "discard Avengers Mansion" (isTargetChoice $ supportIdOf mansion)
      getsGame (isJust . lookup (supportIdOf mansion) . gameSupports) `shouldReturn` False

  for_ [False,True] $ \tech ->
    it ("Supersonic Punch schemes and confuses only with Tech: " <> show tech) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        _ <- withLeader "56059"
        when tech $ do
          boots <- encounterCard C.cw56066
          resolve [RevealEncounterCard you boots]
        _ <- stackEncounterDeck [M.hydraMercenary]
        punch <- encounterCard C.cw56071
        before <- mainSchemeThreatNow
        resolve [RevealEncounterCard you punch]
        mainSchemeThreatNow `shouldReturn` (before + if tech then 3 else 2)
        isConfused you `shouldReturn` tech

  for_ [False,True] $ \tech ->
    it ("Supersonic Punch gains piercing only with Tech: " <> show tech) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        changeForm you
        _ <- withLeader "56059"
        when tech $ do
          boots <- encounterCard C.cw56066
          resolve [RevealEncounterCard you boots]
        resolve [GiveStatus (IdentityRef you) ToughStatus]
        _ <- stackEncounterDeck [M.hydraMercenary]
        punch <- encounterCard C.cw56071
        resolve [RevealEncounterCard you punch]
        damageOf you `shouldReturn` (if tech then 3 else 0)

  it "Photonic Blast attacks and adds an energy counter after the attack" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- withLeader "56092"
      energy <- encounterCard C.cw56098
      resolve [RevealEncounterCard you energy]
      _ <- stackEncounterDeck [M.hydraMercenary]
      blast <- encounterCard C.cw56101
      resolve [RevealEncounterCard you blast]
      damageOf you `shouldReturn` 3
      getCountersOn (attachmentIdOf energy) `shouldReturn` 1

  it "Photonic Blast discards an ally or support in alter-ego form" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "56092"
      cat <- putIntoPlay you "01067"
      blast <- encounterCard C.cw56101
      start [RevealEncounterCard you blast]
      choose "discard Black Cat" (isTargetChoice $ allyIdOf cat)
      selectAny (AllyWithId $ allyIdOf cat) `shouldReturn` False

  it "Crisis Interdiction places two threat on every scheme in hero form" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- withLeader "56092"
      crowd <- encounterCard S.crowdControl
      resolve [RevealEncounterCard you crowd]
      before <- mainSchemeThreatNow
      let sid = SideSchemeId $ unCardId $ ecCardId crowd
      total <- L.sideSchemeThreatOf sid
      crisis <- encounterCard C.cw56102
      resolve [RevealEncounterCard you crisis]
      mainSchemeThreatNow `shouldReturn` (before + 2)
      L.sideSchemeThreatOf sid `shouldReturn` (total + 2)

  it "Crisis Interdiction schemes and adds energy in alter-ego form" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "56092"
      energy <- encounterCard C.cw56098
      resolve [RevealEncounterCard you energy]
      _ <- stackEncounterDeck [M.hydraMercenary]
      crisis <- encounterCard C.cw56102
      before <- mainSchemeThreatNow
      resolve [RevealEncounterCard you crisis]
      mainSchemeThreatNow `shouldReturn` (before + 2)
      getCountersOn (attachmentIdOf energy) `shouldReturn` 1

  it "Energy Absorption heals using the updated energy total" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "56092"
      energy <- encounterCard C.cw56098
      resolve [RevealEncounterCard you energy,PlaceCounters (AttachmentRef $ attachmentIdOf energy) 2,VillainMessage vid $ VillainDamaged GameRef (toDamage 5 FromAbility)]
      absorption <- encounterCard C.cw56103
      resolve [RevealEncounterCard you absorption]
      getCountersOn (attachmentIdOf energy) `shouldReturn` 3
      leaderHp vid `shouldReturn` 12

  it "Energy Absorption's boost places one energy counter" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "56092"
      energy <- encounterCard C.cw56098
      resolve [RevealEncounterCard you energy]
      absorption <- encounterCard C.cw56103
      resolve [RevealBoostCard absorption (EnemyVillainId vid),ClearBoosts]
      getCountersOn (attachmentIdOf energy) `shouldReturn` 1
