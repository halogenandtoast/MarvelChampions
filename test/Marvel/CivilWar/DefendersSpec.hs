module Marvel.CivilWar.DefendersSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Keyword qualified as K
import Marvel.Message.Lifted qualified as L
import Marvel.Matchers
import Marvel.Minion.Cards qualified as M
import Marvel.Modifier
import Marvel.Query (selectList)
import TestImport

spec :: Spec
spec = describe "Civil War Defenders" do
  for_ [(C.cw56184,StunnedStatus),(C.cw56185,ConfusedStatus)] $ \(def,status) -> do
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

  for_ [False,True] $ \already ->
    it ("Protect the Innocent gives the leader and all minions tough and checks its surge condition: " <> show already) do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        let p = player1 tg
            q = player2 tg
        vid <- villainIdNow
        first <- addMinion p M.hydraSoldier
        second <- addMinion q M.hydraSoldier
        when already $ resolve [GiveStatus (VillainRef vid) ToughStatus]
        [next,_] <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier]
        card <- encounterCard C.cw56186
        resolve [RevealEncounterCard p card]
        for_ [VillainCharacter vid,MinionCharacter first,MinionCharacter second] $ \target ->
          getCharacterStatusCount target ToughStatus `shouldReturn` 1
        playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` if already then [next] else []

  it "Protect the Innocent ignores the other board's leader and minions" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      own <- villainIdNow
      friendly <- addSecondBoard tg
      ownMinion <- addMinion p M.hydraSoldier
      otherMinion <- addMinion q M.hydraSoldier
      resolve [GiveStatus (VillainRef friendly) ToughStatus]
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier]
      card <- encounterCard C.cw56186
      resolve [RevealEncounterCard p card]
      getCharacterStatusCount (VillainCharacter own) ToughStatus `shouldReturn` 1
      getCharacterStatusCount (MinionCharacter ownMinion) ToughStatus `shouldReturn` 1
      getCharacterStatusCount (MinionCharacter otherMinion) ToughStatus `shouldReturn` 0
      playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` []

  for_ [C.cw56187,C.cw56192] $ \def -> do
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

  it "The Defenders uses hinder, grants guard, and contributes an acceleration icon" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you M.shocker
      before <- mainSchemeThreat
      resolve [AccelerateMainScheme]
      baseline <- mainSchemeThreat
      side <- encounterCard C.cw56188
      resolve [RevealEncounterCard you side]
      let sid = SideSchemeId $ unCardId $ ecCardId side
      sideSchemeThreat sid `shouldReturn` 5
      getEnemyKeywords (EnemyMinionId mid) >>= (`shouldSatisfy` elem K.Guard)
      resolve [AccelerateMainScheme]
      after <- mainSchemeThreat
      (after - baseline) `shouldBe` (baseline - before + 1)
      resolve [RemoveFromPlay $ SideSchemeRef sid]
      getEnemyKeywords (EnemyMinionId mid) >>= (`shouldSatisfy` notElem K.Guard)
