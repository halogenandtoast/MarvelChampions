module Marvel.CivilWar.HeroesForHireSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Message.Lifted qualified as L
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Minion.Cards qualified as M
import TestImport

spec :: Spec
spec = describe "Civil War Heroes for Hire" do
  for_ [(C.cw56112,["01002","01050"],AllyType),(C.cw56113,["01008","01057"],UpgradeType)] $ \(def,codes,kind) -> do
    it (show (toCardCode def) <> " lets a registered player choose their controlled card to discard") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        cards <- traverse (putInHand you) codes
        setup <- L.runQueueT $ L.capture $ traverse_ (L.putIntoPlay you) cards
        resolve setup
        card <- encounterCard def
        start [RevealEncounterCard you card]
        fmap fst <$> asked `shouldReturn` Just you
        let selected = cards !! 1
            target = if kind == AllyType then AllyRef (allyIdOf selected) else UpgradeRef (upgradeIdOf selected)
        choose "discard the chosen controlled card" (isTargetChoice target)
        pass
        fmap (map pcCardId) (discardOf you) >>= (`shouldSatisfy` elem (pcCardId selected))

    it (show (toCardCode def) <> " gives an Unregistered player's discard choice to the opposing team") do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let p = player1 tg
            q = player2 tg
        _ <- addSecondBoard tg
        obligation <- encounterCard C.cw56114
        resolve [RevealEncounterCard p obligation]
        cards <- traverse (putInHand p) codes
        setup <- L.runQueueT $ L.capture $ traverse_ (L.putIntoPlay p) cards
        resolve setup
        card <- encounterCard def
        start [RevealEncounterCard p card]
        fmap fst <$> asked `shouldReturn` Just q
        let selected = testHead cards
            target = if kind == AllyType then AllyRef (allyIdOf selected) else UpgradeRef (upgradeIdOf selected)
        choose "opponent discards the selected card" (isTargetChoice target)
        pass
        fmap (map pcCardId) (discardOf p) >>= (`shouldSatisfy` elem (pcCardId selected))
        discardOf q `shouldReturn` []

    it (show (toCardCode def) <> " asks no discard question with no matching controlled card") do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- addMinion (player1 tg) def
        offers (== EndTurn) `shouldReturn` True

  for_ [(C.cw56112,["01002","01050"],AllyType),(C.cw56113,["01008","01057"],UpgradeType)] $ \(def,codes,kind) ->
    it (show (toCardCode def) <> " excludes controlled cards that cannot be discarded") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        cards <- traverse (putInHand you) codes
        let protected = testHead cards
            selected = cards !! 1
            target card = if kind == AllyType then AllyRef (allyIdOf card) else UpgradeRef (upgradeIdOf card)
        setup <- L.runQueueT $ L.capture $ do
          traverse_ (L.putIntoPlay you) cards
          L.lastingModifiers GameRef (SourceEntity $ toSource $ target protected) [CannotBeDiscarded] False
        resolve setup
        card <- encounterCard def
        start [RevealEncounterCard you card]
        offers (isTargetChoice $ target protected) `shouldReturn` False
        choose "discard the eligible card" (isTargetChoice $ target selected)
        pass
        getsGame (`cardInPlay` target protected) >>= (`shouldSatisfy` isJust)
        fmap (map pcCardId) (discardOf you) >>= (`shouldSatisfy` elem (pcCardId selected))

  for_ [C.cw56115,C.cw56119] $ \def -> do
    for_ [("01088",2),("01086",1),("01055",1)] $ \(code,icons) ->
      it (show (toCardCode def) <> " places threat equal to every printed resource icon on " <> show code) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let you = player1 tg
          clearHand you
          kept <- putInHand you "01090"
          discarded <- putInHand you code
          card <- encounterCard def
          start [RevealEncounterCard you card]
          choose "discard the resource card" (isTargetChoice discarded)
          pass
          mainSchemeThreat `shouldReturn` icons
          handOf you `shouldReturn` [kept]
          fmap (map pcCardId) (discardOf you) `shouldReturn` [pcCardId discarded]

    it (show (toCardCode def) <> " resolves without a question or threat when the hand is empty") do
      tg <- testGame ["spider-man"]
      inGame tg do
        clearHand (player1 tg)
        card <- encounterCard def
        resolve [RevealEncounterCard (player1 tg) card]
        mainSchemeThreat `shouldReturn` 0
        offers (== EndTurn) `shouldReturn` True

    for_ [False,True] $ \unregistered ->
      it (show (toCardCode def) <> " handles System Shock's choice restriction: Unregistered " <> show unregistered) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let you = player1 tg
          when unregistered $ do
            obligation <- encounterCard C.cw56114
            resolve [RevealEncounterCard you obligation]
          clearHand you
          shock <- putInHand you "21185"
          card <- encounterCard def
          resolve [RevealEncounterCard you card]
          handOf you `shouldReturn` if unregistered then [] else [shock]
          fmap (map pcCardId) (discardOf you) `shouldReturn` if unregistered then [pcCardId shock] else []
          mainSchemeThreat `shouldReturn` 0

  it "Bounty Hunting's random discard repeats after restoring the saved game and generator" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      obligation <- encounterCard C.cw56114
      resolve [RevealEncounterCard you obligation]
      clearHand you
      _ <- traverse (putInHand you) ["01088","01086","01090","01008"]
      card <- encounterCard C.cw56115
      (saved,queue,generator) <- liftIO $ currentState tg.app
      resolve [RevealEncounterCard you card]
      first <- map pcCardId <$> discardOf you
      threat <- mainSchemeThreat
      case fromJSON (toJSON saved) of
        Success restored -> withGame_ $ const restored
        Error reason -> error reason
      liftIO $ writeIORef tg.app.appQueue queue
      liftIO $ writeIORef tg.app.appRandomGen generator
      resolve [RevealEncounterCard you card]
      map pcCardId <$> discardOf you `shouldReturn` first
      mainSchemeThreat `shouldReturn` threat
      length first `shouldBe` 1

  it "Heroes for Hire starts with hinder threat and its hazard icon" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      side <- encounterCard C.cw56116
      resolve [RevealEncounterCard (player1 tg) side]
      sideSchemeThreat (SideSchemeId $ unCardId $ ecCardId side) `shouldReturn` 5
      cdHazards (getCardDef side) `shouldBe` 1

  it "Heroes for Hire deals Unregistered Super to the defeating player" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      (_,discard) <- setEncounterDeck [M.hydraMercenary,M.hydraSoldier] [C.cw56114]
      side <- encounterCard C.cw56116
      resolve [RevealEncounterCard p side]
      let sid = SideSchemeId $ unCardId $ ecCardId side
      resolve [SideSchemeMessage sid $ SideSchemeThwarted (IdentityRef q) 5]
      playerIdentityEncounterCards <$> playerAttrs q `shouldReturn` discard
      playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` []

  it "Heroes for Hire does not ask an empty search question when no Unregistered Super exists" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- setEncounterDeck [M.hydraMercenary,M.hydraSoldier] []
      side <- encounterCard C.cw56116
      resolve [RevealEncounterCard you side]
      let sid = SideSchemeId $ unCardId $ ecCardId side
      resolve [SideSchemeMessage sid $ SideSchemeThwarted (IdentityRef you) 4]
      getsGame (HashMap.member sid . gameSideSchemes) `shouldReturn` False
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` []
