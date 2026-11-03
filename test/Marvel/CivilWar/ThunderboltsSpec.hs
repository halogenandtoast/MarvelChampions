module Marvel.CivilWar.ThunderboltsSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Keyword qualified as K
import Marvel.Message.Lifted qualified as L
import Marvel.Matchers
import Marvel.Minion.Cards qualified as M
import Marvel.Modifier
import Marvel.Query (selectList)
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet qualified as E
import Test.Hspec qualified as H
import TestImport

spec :: Spec
spec = describe "Synthezoid Thunderbolts" do
  for_ [(C.cw57017,StunnedStatus),(C.cw57018,ConfusedStatus)] $ \(def,status) -> do
    it (show (toCardCode def) <> " gives the revealing identity its status in cooperative play") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        _ <- addMinion you def
        getCharacterStatusCount (IdentityCharacter you) status `shouldReturn` 1

    it (show (toCardCode def) <> " gives the status to the friendly leader across the table") do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        leader <- addSecondBoard tg
        _ <- addMinion (player1 tg) def
        getCharacterStatusCount (VillainCharacter leader) status `shouldReturn` 1
        getCharacterStatusCount (IdentityCharacter $ player1 tg) status `shouldReturn` 0

    for_ [False,True] $ \stalwart ->
      it (show (toCardCode def) <> " uses otherwise if the leader cannot receive a new status: stalwart " <> show stalwart) do
        tg <- competitive ["spider-man","captain-marvel"]
        inGame tg do
          let you = player1 tg
          leader <- addSecondBoard tg
          if stalwart then do
            immune <- L.runQueueT $ L.capture $ L.lastingModifiers GameRef (SourceEntity $ VillainRef leader) [KeywordModifier K.Stalwart] False
            resolve immune
          else resolve [GiveStatus (VillainRef leader) status]
          _ <- addMinion you def
          getCharacterStatusCount (IdentityCharacter you) status `shouldReturn` 1
          getCharacterStatusCount (VillainCharacter leader) status `shouldReturn` if stalwart then 0 else 1

    it (show (toCardCode def) <> " can give a steady leader its second status instead of affecting the identity") do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        leader <- addSecondBoard tg
        steady <- L.runQueueT $ L.capture $ L.lastingModifiers GameRef (SourceEntity $ VillainRef leader) [KeywordModifier K.Steady] False
        resolve steady
        resolve [GiveStatus (VillainRef leader) status]
        _ <- addMinion you def
        getCharacterStatusCount (VillainCharacter leader) status `shouldReturn` 2
        getCharacterStatusCount (IdentityCharacter you) status `shouldReturn` 0

    it (show (toCardCode def) <> " resolves the same status as a boost without entering play") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        vid <- withLeader "57040"
        _ <- stackEncounterDeck [def,M.hydraMercenary]
        resolve [VillainMessage vid VillainSchemes]
        getCharacterStatusCount (IdentityCharacter you) status `shouldReturn` 1
        mainSchemeThreat `shouldReturn` 1
        asYou you (selectList AnyMinion) `shouldReturn` []

    it (show (toCardCode def) <> " retains its boost callback long enough to resolve otherwise") do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        own <- withLeader "57040"
        friendly <- addSecondBoard tg
        resolve [GiveStatus (VillainRef friendly) status]
        _ <- stackEncounterDeck [def,M.hydraMercenary]
        resolve [VillainMessage own VillainSchemes]
        getCharacterStatusCount (IdentityCharacter you) status `shouldReturn` 1
        getCharacterStatusCount (VillainCharacter friendly) status `shouldReturn` 1


  it "gathers the five physical cards" do
    cards <- gatherEncounterSet E.CwThunderboltsModular
    length cards `H.shouldBe` 5

  for_ [False,True] $ \boost -> do
    it ("Penance damages a controlled character without a friendly leader: boost " <> show boost) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        if boost then do
          own <- withLeader "57040"
          _ <- stackEncounterDeck [C.cw57019,M.hydraMercenary]
          start [VillainMessage own VillainSchemes]
        else do
          card <- encounterCard C.cw57019
          start [RevealEncounterCard you card]
        choose "damage the identity" (isTargetChoice $ IdentityCharacter you)
        pass
        damageOf you `shouldReturn` if boost then 1 else 2

    for_ [False,True] $ \tough ->
      it ("Penance deals two damage to the friendly leader: boost " <> show boost <> ", tough " <> show tough) do
        tg <- competitive ["spider-man","captain-marvel"]
        inGame tg do
          let you = player1 tg
          own <- withLeader "57040"
          friendly <- addSecondBoard tg
          before <- getCharacterRemainingHitPoints (VillainCharacter friendly)
          when tough $ resolve [GiveStatus (VillainRef friendly) ToughStatus]
          if boost then do
            _ <- stackEncounterDeck [C.cw57019,M.hydraMercenary]
            resolve [VillainMessage own VillainSchemes]
          else do
            card <- encounterCard C.cw57019
            resolve [RevealEncounterCard you card]
          getCharacterRemainingHitPoints (VillainCharacter friendly) `shouldReturn` (before - if tough then 0 else 2)
          getCharacterStatusCount (VillainCharacter friendly) ToughStatus `shouldReturn` 0
          damageOf you `shouldReturn` 0

  for_ [False,True] $ \opposing ->
    it ("The Thunderbolts protects only its board: opposing minion " <> show opposing) do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        _ <- addSecondBoard tg
        _ <- addMinion (if opposing then player2 tg else you) C.cw57017
        card <- encounterCard C.cw57021
        resolve [RevealEncounterCard you card]
        let sid = SideSchemeId $ unCardId $ ecCardId card
        sideSchemeThreat sid `shouldReturn` 4
        modifiers <- getModifiers (SideSchemeRef sid)
        (ThreatCannotBeRemoved `elem` modifiers) `shouldBe` not opposing

  for_ [C.cw57020] $ \def -> do
    for_ [False,True] $ \hero ->
      it (show (toCardCode def) <> " activates the leader without a boost when there are no minions: hero " <> show hero) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let you = player1 tg
          _ <- withLeader "57040"
          when hero $ changeForm you
          [next,_] <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier]
          card <- encounterCard def
          resolve [RevealEncounterCard you card]
          damageOf you `shouldReturn` if hero then 1 else 0
          mainSchemeThreat `shouldReturn` if hero then 0 else 1
          headMay <$> getEncounterDeck `shouldReturn` Just next

    it (show (toCardCode def) <> " activates each minion against its engaged player") do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        let p = player1 tg
            q = player2 tg
        _ <- withLeader "57040"
        _ <- addMinion p M.hydraSoldier
        _ <- addMinion q M.hydraSoldier
        [next,_] <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier]
        card <- encounterCard def
        resolve [RevealEncounterCard p card]
        mainSchemeThreat `shouldReturn` 2
        headMay <$> getEncounterDeck `shouldReturn` Just next

    it (show (toCardCode def) <> " counts a confused minion's status replacement without activating the leader") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        _ <- withLeader "57040"
        mid <- addMinion you M.hydraSoldier
        resolve [GiveStatus (MinionRef mid) ConfusedStatus]
        card <- encounterCard def
        resolve [RevealEncounterCard you card]
        mainSchemeThreat `shouldReturn` 0
        getCharacterStatusCount (MinionCharacter mid) ConfusedStatus `shouldReturn` 0

