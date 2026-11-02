module Marvel.CivilWar.VisionTreacherySpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader,attachmentIdOf)
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Minion.Cards qualified as M
import Marvel.Query
import TestImport

formCode :: EncounterCard -> GameAppT (Maybe CardCode)
formCode card = getsGame $ fmap (toCardCode . getCardDef) . lookup (attachmentIdOf card) . gameAttachments

spec :: Spec
spec = describe "Vision leader treacheries" do
  for_ [(False,False,2),(False,True,4),(True,False,4),(True,True,2)] $ \(hero,intangible,amount) ->
    it ("Solar Beam modifies only the matching activation: hero " <> show hero <> ", intangible " <> show intangible) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        when hero $ changeForm you
        vid <- withLeader "57040"
        form <- encounterCard $ if intangible then C.cw57046b else C.cw57046a
        resolve [RevealEncounterCard you form]
        _ <- stackEncounterDeck [M.hydraMercenary]
        beam <- encounterCard C.cw57051
        before <- mainSchemeThreatNow
        resolve [RevealEncounterCard you beam]
        if hero then damageOf you `shouldReturn` amount else mainSchemeThreatNow `shouldReturn` (before + amount)
        getEnemyScheme (EnemyVillainId vid) `shouldReturn` (if intangible then 2 else 1)
        getVillainAttackValue vid `shouldReturn` (if intangible then 1 else 2)

  it "Solar Beam observes Density Control's form change before the attack begins" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- withLeader "57040"
      form <- encounterCard C.cw57046b
      density <- encounterCard C.cw57050
      resolve [RevealEncounterCard you form,PutEncounterCardIntoPlay you density]
      _ <- stackEncounterDeck [M.hydraMercenary]
      beam <- encounterCard C.cw57051
      resolve [RevealEncounterCard you beam]
      damageOf you `shouldReturn` 4
      formCode form `shouldReturn` Just "57046a"

  it "preserves Solar Beam's actual ATK modifier when saved during defense" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      vid <- withLeader "57040"
      form <- encounterCard C.cw57046a
      resolve [RevealEncounterCard you form]
      _ <- stackEncounterDeck [M.hydraMercenary]
      beam <- encounterCard C.cw57051
      start [RevealEncounterCard you beam]
      let declineOptional = do
            offered <- offers (isLabel "$label.noResponse")
            when offered $ choose "decline the optional response" (isLabel "$label.noResponse") >> declineOptional
      declineOptional
      getVillainAttackValue vid `shouldReturn` 3
      saved <- getsGame toJSON
      case fromJSON @Game saved of
        Success restored -> withGame_ $ const restored
        Error reason -> liftIO $ expectationFailure reason
      getVillainAttackValue vid `shouldReturn` 3
      pass
      damageOf you `shouldReturn` 4
      getVillainAttackValue vid `shouldReturn` 2

  it "Superdense Strike can damage an exhausted character while Dense is in play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "57040"
      form <- encounterCard C.cw57046a
      resolve [RevealEncounterCard you form,IdentityMessage you ExhaustedIdentity]
      strike <- encounterCard C.cw57052
      start [RevealEncounterCard you strike]
      choose "choose the exhausted identity" (isTargetChoice you)
      damageOf you `shouldReturn` 2
      formCode form `shouldReturn` Just "57046a"

  it "Superdense Strike exhausts a chosen character and flips Intangible" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "57040"
      form <- encounterCard C.cw57046b
      resolve [RevealEncounterCard you form]
      strike <- encounterCard C.cw57052
      start [RevealEncounterCard you strike]
      choose "exhaust the identity" (isTargetChoice you)
      isExhaustedNow you `shouldReturn` True
      damageOf you `shouldReturn` 0
      formCode form `shouldReturn` Just "57046a"

  it "Superdense Strike flips Intangible even if no character can be exhausted" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "57040"
      form <- encounterCard C.cw57046b
      resolve [RevealEncounterCard you form,IdentityMessage you ExhaustedIdentity]
      strike <- encounterCard C.cw57052
      resolve [RevealEncounterCard you strike]
      damageOf you `shouldReturn` 0
      formCode form `shouldReturn` Just "57046a"

  for_ [False,True] $ \intangible ->
    it ("Phase Disruption chooses a controlled upgrade: intangible " <> show intangible) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        _ <- withLeader "57040"
        form <- encounterCard $ if intangible then C.cw57046b else C.cw57046a
        resolve [RevealEncounterCard you form]
        upgrade <- putIntoPlay you "01093"
        let uid = UpgradeId $ unCardId $ pcCardId upgrade
        when intangible $ resolve [UpgradeMessage uid ExhaustedUpgrade]
        disruption <- encounterCard C.cw57053
        start [RevealEncounterCard you disruption]
        choose "the enemy team chooses Tenacity" (isTargetChoice uid)
        if intangible then selectAny (UpgradeWithId uid) `shouldReturn` False
          else getsGame (maybe False (upgradeExhausted . toAttrs) . lookup uid . gameUpgrades) `shouldReturn` True
        formCode form `shouldReturn` Just "57046b"

  it "Phase Disruption flips Dense even when there are no upgrades to exhaust" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "57040"
      form <- encounterCard C.cw57046a
      resolve [RevealEncounterCard you form]
      disruption <- encounterCard C.cw57053
      resolve [RevealEncounterCard you disruption]
      formCode form `shouldReturn` Just "57046b"
