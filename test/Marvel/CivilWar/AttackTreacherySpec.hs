module Marvel.CivilWar.AttackTreacherySpec (spec) where

import Marvel.Attack qualified as A
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader,attachmentIdOf,leaderHp)
import Marvel.Minion.Cards qualified as M
import TestImport

declineOptional :: GameAppT ()
declineOptional = do
  offered <- offers (isLabel "$label.noResponse")
  when offered $ choose "decline the optional response" (isLabel "$label.noResponse") >> declineOptional

spec :: Spec
spec = describe "Leader attack treacheries" do
  it "Heroic Strike stuns the hero actually damaged, including after saving during defense" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- withLeader "56137"
      _ <- stackEncounterDeck [M.hydraMercenary]
      strike <- encounterCard C.cw56148
      start [RevealEncounterCard you strike]
      declineOptional
      saved <- getsGame toJSON
      case fromJSON @Game saved of
        Success restored -> withGame_ $ const restored
        Error reason -> liftIO $ expectationFailure reason
      pass
      damageOf you `shouldReturn` 3
      isStunned you `shouldReturn` True

  it "Heroic Strike does not stun a hero whose tough prevented the damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- withLeader "56137"
      resolve [GiveStatus (IdentityRef you) ToughStatus]
      _ <- stackEncounterDeck [M.hydraMercenary]
      strike <- encounterCard C.cw56148
      resolve [RevealEncounterCard you strike]
      damageOf you `shouldReturn` 0
      isStunned you `shouldReturn` False

  it "Heroic Strike stuns the hero damaged by its overkill, without targeting a defeated ally" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- withLeader "56137"
      serum <- encounterCard C.cw56145
      resolve [RevealEncounterCard you serum]
      fury <- putIntoPlay you "01084"
      _ <- stackEncounterDeck [M.hydraMercenary]
      strike <- encounterCard C.cw56148
      start [RevealEncounterCard you strike]
      declineOptional
      choose "give this attack overkill" (isAbilityOf $ attachmentIdOf serum)
      choose "defend with Nick Fury" $ \case AllyDefend aid _ -> aid == allyIdOf fury; _ -> False
      pass
      damageOf you `shouldReturn` 2
      isStunned you `shouldReturn` True

  it "Heroic Strike discards an ally or upgrade in alter-ego form" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "56137"
      upgrade <- putIntoPlay you "01093"
      let uid = UpgradeId $ unCardId $ pcCardId upgrade
      strike <- encounterCard C.cw56148
      start [RevealEncounterCard you strike]
      choose "discard Tenacity" (isTargetChoice uid)
      present <- getsGame $ isJust . lookup uid . gameUpgrades
      liftIO $ present `shouldBe` False
      isStunned you `shouldReturn` False

  it "Venom Blast surges in alter-ego form" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "56168"
      [surged] <- stackEncounterDeck [M.hydraMercenary]
      blast <- encounterCard C.cw56177
      resolve [RevealEncounterCard you blast]
      dealtTo you >>= (`shouldSatisfy` elem surged)

  it "Venom Blast gains ranged from the counter Finesse adds as it is revealed" do
    tg <- testGame ["black-panther"]
    inGame tg do
      let you = player1 tg
      changeForm you
      vid <- withLeader "56168"
      finesse <- encounterCard C.cw56174
      resolve [RevealEncounterCard you finesse]
      before <- leaderHp vid
      _ <- stackEncounterDeck [M.hydraMercenary]
      blast <- encounterCard C.cw56177
      resolve [RevealEncounterCard you blast]
      damageOf you `shouldReturn` 4
      leaderHp vid `shouldReturn` before
      getCountersOn (attachmentIdOf finesse) `shouldReturn` 0

  it "Venom Blast gains piercing from the two skill counters actually removed" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- withLeader "56168"
      finesse <- encounterCard C.cw56174
      resolve [RevealEncounterCard you finesse,PlaceCounters (AttachmentRef $ attachmentIdOf finesse) 1,GiveStatus (IdentityRef you) ToughStatus]
      _ <- stackEncounterDeck [M.hydraMercenary]
      blast <- encounterCard C.cw56177
      resolve [RevealEncounterCard you blast]
      damageOf you `shouldReturn` 5
      getCountersOn (attachmentIdOf finesse) `shouldReturn` 0

  it "Venom Blast gains all three keywords and overkills an ally at three skill counters" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      vid <- withLeader "56168"
      finesse <- encounterCard C.cw56174
      resolve [RevealEncounterCard you finesse,PlaceCounters (AttachmentRef $ attachmentIdOf finesse) 2]
      fury <- putIntoPlay you "01084"
      _ <- stackEncounterDeck [M.hydraMercenary]
      blast <- encounterCard C.cw56177
      start [RevealEncounterCard you blast]
      choose "Finesse gains its third counter" (isAbilityOf $ attachmentIdOf finesse)
      declineOptional
      choose "spend Finesse for Spider-Woman's attack" (isAbilityOf vid)
      attack <- getEnemyAttack $ EnemyVillainId vid
      liftIO $ fmap (\a -> (A.attackRanged a,A.attackPiercing a,A.attackOverkill a)) attack `shouldBe` Just (True,True,True)
      choose "defend with Nick Fury" $ \case AllyDefend aid _ -> aid == allyIdOf fury; _ -> False
      pass
      damageOf you `shouldReturn` 3
      getCountersOn (attachmentIdOf finesse) `shouldReturn` 0
