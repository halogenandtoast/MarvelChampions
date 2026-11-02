module Marvel.CivilWar.LeaderTreacherySpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader,attachmentIdOf,leaderHp)
import Marvel.Message.Lifted qualified as L
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Minion.Cards qualified as M
import Marvel.Query
import Marvel.SideScheme.Cards qualified as S
import Marvel.Villain.Types qualified as V
import TestImport hiding (ExhaustedAlly)

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

  for_ [False,True] $ \heldByHero ->
    it ("Shield Block returns the actual Shield and deals a boost: hero holds it " <> show heldByHero) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        vid <- withLeader "56137"
        shield <- encounterCard C.cw56143
        resolve [RevealEncounterCard you shield]
        when heldByHero $ do
          messages <- L.runQueueT $ L.capture $ L.attachToCharacter (attachmentIdOf shield) (IdentityCharacter you)
          resolve messages
        resolve [RemoveStatus (VillainRef vid) ToughStatus]
        _ <- stackEncounterDeck [M.hydraMercenary]
        block <- encounterCard C.cw56146
        resolve [RevealEncounterCard you block]
        getAttachedCharacter (attachmentIdOf shield) `shouldReturn` Just (VillainCharacter vid)
        villainMatches VillainWithToughStatus vid `shouldReturn` True
        boosts <- getsGame $ maybe 0 (length . V.villainBoostCards . toAttrs) . lookup vid . gameVillains
        liftIO $ boosts `shouldBe` 1

  it "Shield Toss damages the hero and each ally when Captain America has his Shield" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- withLeader "56137"
      shield <- encounterCard C.cw56143
      resolve [RevealEncounterCard you shield]
      cat <- putIntoPlay you "01067"
      toss <- encounterCard C.cw56147
      resolve [RevealEncounterCard you toss]
      damageOf you `shouldReturn` 2
      selectAny (AllyWithId $ allyIdOf cat) `shouldReturn` False

  it "Shield Toss returns a stolen Shield without dealing its damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      vid <- withLeader "56137"
      shield <- encounterCard C.cw56143
      resolve [RevealEncounterCard you shield]
      messages <- L.runQueueT $ L.capture $ L.attachToCharacter (attachmentIdOf shield) (IdentityCharacter you)
      resolve messages
      toss <- encounterCard C.cw56147
      resolve [RevealEncounterCard you toss]
      getAttachedCharacter (attachmentIdOf shield) `shouldReturn` Just (VillainCharacter vid)
      damageOf you `shouldReturn` 0

  it "Shield Toss schemes against an alter-ego" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- withLeader "56137"
      _ <- stackEncounterDeck [M.hydraMercenary]
      toss <- encounterCard C.cw56147
      before <- mainSchemeThreatNow
      resolve [RevealEncounterCard (player1 tg) toss]
      mainSchemeThreatNow `shouldReturn` (before + 2)

  for_ [0,2] $ \initial ->
    it ("Pheromones stuns, confuses and respects Finesse's three-counter limit from " <> show initial) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        _ <- withLeader "56168"
        finesse <- encounterCard C.cw56174
        resolve [RevealEncounterCard you finesse,PlaceCounters (AttachmentRef $ attachmentIdOf finesse) initial]
        pheromones <- encounterCard C.cw56176
        resolve [RevealEncounterCard you pheromones]
        isStunned you `shouldReturn` True
        isConfused you `shouldReturn` True
        getCountersOn (attachmentIdOf finesse) `shouldReturn` min 3 (initial + 2)

  for_ [False,True] $ \alreadyStunned ->
    it ("Pheromones's boost confuses only if it cannot stun: " <> show alreadyStunned) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        vid <- withLeader "56168"
        when alreadyStunned $ resolve [GiveStatus (IdentityRef you) StunnedStatus]
        pheromones <- encounterCard C.cw56176
        resolve [RevealBoostCard pheromones (EnemyVillainId vid),ClearBoosts]
        isStunned you `shouldReturn` True
        isConfused you `shouldReturn` alreadyStunned

  for_ [False,True] $ \hero ->
    it ("One-Two Punch activates then gives She-Hulk tough: hero " <> show hero) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        when hero $ changeForm you
        vid <- withLeader "57001"
        _ <- stackEncounterDeck [M.hydraMercenary]
        punch <- encounterCard C.cw57009
        before <- mainSchemeThreatNow
        resolve [RevealEncounterCard you punch]
        villainMatches VillainWithToughStatus vid `shouldReturn` True
        if hero then damageOf you `shouldReturn` 3 else mainSchemeThreatNow `shouldReturn` (before + 2)

  it "Ground Stomp exhausts the identity and all ready allies" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "57001"
      cat <- putIntoPlay you "01067"
      stomp <- encounterCard C.cw57010
      resolve [RevealEncounterCard you stomp]
      isExhaustedNow you `shouldReturn` True
      allyMatches ExhaustedAlly (allyIdOf cat) `shouldReturn` True

  it "Ground Stomp surges if every controlled character was already exhausted" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "57001"
      resolve [IdentityMessage you ExhaustedIdentity]
      [surged] <- stackEncounterDeck [S.crowdControl]
      stomp <- encounterCard C.cw57010
      resolve [RevealEncounterCard you stomp]
      dealtTo you >>= (`shouldSatisfy` elem surged)

  it "Ground Stomp's boost lets its resolver choose a ready character to exhaust" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57001"
      cat <- putIntoPlay you "01067"
      stomp <- encounterCard C.cw57010
      start [RevealBoostCard stomp (EnemyVillainId vid),ClearBoosts]
      choose "exhaust Black Cat" (isTargetChoice $ allyIdOf cat)
      isExhaustedNow you `shouldReturn` False
      allyMatches ExhaustedAlly (allyIdOf cat) `shouldReturn` True

  it "Inconspicuous offers the highest-cost controlled card rather than a cheaper one" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "56168"
      mansion <- putIntoPlay you "01091"
      tenacity <- putIntoPlay you "01093"
      inconspicuous <- encounterCard C.cw56178
      start [RevealEncounterCard you inconspicuous]
      choose "discard the highest-cost card" (isLabel "$label.cwDiscardHighestCost")
      pass
      getsGame (isJust . lookup (supportIdOf mansion) . gameSupports) `shouldReturn` False
      getsGame (isJust . lookup (UpgradeId $ unCardId $ pcCardId tenacity) . gameUpgrades) `shouldReturn` True

  it "Inconspicuous offers scheming when there is no controlled card to discard" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- withLeader "56168"
      _ <- stackEncounterDeck [M.hydraMercenary]
      before <- mainSchemeThreatNow
      inconspicuous <- encounterCard C.cw56178
      start [RevealEncounterCard (player1 tg) inconspicuous]
      offers (isLabel "$label.cwDiscardHighestCost") `shouldReturn` False
      choose "Spider-Woman schemes" (isLabel "$label.cwSpiderWomanSchemes")
      pass
      mainSchemeThreatNow `shouldReturn` (before + 2)

  it "Inconspicuous's boost counts all three status cards on the resolving identity" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "56168"
      resolve [GiveStatus (IdentityRef you) StunnedStatus,GiveStatus (IdentityRef you) ConfusedStatus,GiveStatus (IdentityRef you) ToughStatus]
      inconspicuous <- encounterCard C.cw56178
      resolve [RevealBoostCard inconspicuous (EnemyVillainId vid)]
      n <- getsGame $ maybe 0 (V.villainBoost . toAttrs) . lookup vid . gameVillains
      liftIO $ n `shouldBe` 3
