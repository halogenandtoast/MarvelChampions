module Marvel.CivilWar.RoyalGuardSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet qualified as E
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.Trait qualified as T
import Test.Hspec qualified as H
import TestImport

setPlayerDeck :: IdentityId -> [PlayerCard] -> GameAppT ()
setPlayerDeck you cards = withGame_ $ entitiesL . playersL . ix you %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDeck = Deck cards}

spec :: Spec
spec = describe "Synthezoid Royal Guard" do
  it "gathers five physical cards" do
    cards <- gatherEncounterSet E.CwRoyalGuard
    length cards `H.shouldBe` 5

  for_ [(C.cw57069,3,1),(C.cw57070,2,2)] $ \(def,atk,sch) -> do
    it (show (toCardCode def) <> " discards two cards without Hunted") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        cards <- stackDeck you ["01088","01086","01008"]
        _ <- addMinion you def
        take 2 <$> discardOf you `shouldReturn` take 2 cards
        mainSchemeThreat `shouldReturn` 0
        damageOf you `shouldReturn` 0

    for_ [False,True] $ \hero ->
      it (show (toCardCode def) <> " activates against its Hunted player: hero " <> show hero) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let you = player1 tg
          debt <- encounterCard C.cw57072
          resolve [RevealEncounterCard you debt]
          when hero $ changeForm you
          before <- unDeck . playerIdentityDeck <$> playerAttrs you
          _ <- addMinion you def
          unDeck . playerIdentityDeck <$> playerAttrs you `shouldReturn` before
          damageOf you `shouldReturn` if hero then atk else 0
          mainSchemeThreat `shouldReturn` if hero then 0 else sch

  it "Blood Debt grants Hunted only to its owner in both forms" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let one = player1 tg
          two = player2 tg
      debt <- encounterCard C.cw57072
      resolve [RevealEncounterCard one debt]
      identityMatches (IdentityWithTrait T.Hunted) one `shouldReturn` True
      identityMatches (IdentityWithTrait T.Hunted) two `shouldReturn` False
      changeForm one
      identityMatches (IdentityWithTrait T.Hunted) one `shouldReturn` True
      offers (isAbilityOf $ ObligationRef $ ObligationId $ unCardId $ ecCardId debt) `shouldReturn` False

  for_ [7,8,9] $ \count ->
    it ("Blood Debt requires all eight cards and discards them as a cost: deck " <> show count) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        cards <- stackDeck you (replicate count "01088")
        setPlayerDeck you cards
        debt <- encounterCard C.cw57072
        resolve [RevealEncounterCard you debt]
        let oid = ObligationId $ unCardId $ ecCardId debt
        offers (isAbilityOf $ ObligationRef oid) `shouldReturn` (count >= 8)
        when (count >= 8) do
          choose "pay Blood Debt" (isAbilityOf $ ObligationRef oid)
          choose "discard eight cards" (isLabel $ countLabel "discardTopOfDeck" 8)
          whenM (offers isFinishPayment) $ choose "finish payment" isFinishPayment
          pass
          identityMatches (IdentityWithTrait T.Hunted) you `shouldReturn` False
          discarded <- discardOf you
          when (count == 9) $ discarded `shouldSatisfy` (\pile -> all (`elem` pile) (take 8 cards))
          getEncounterDiscard >>= (`shouldSatisfy` elem debt)

  for_ [False,True] $ \hero ->
    it ("Atlanteans uses three distinct discarded types in either identity form: hero " <> show hero) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        when hero $ changeForm you
        _ <- stackDeck you ["01088","01086","01008","01002"]
        card <- encounterCard C.cw57071
        resolve [RevealEncounterCard you card]
        damageOf you `shouldReturn` if hero then 3 else 0
        mainSchemeThreat `shouldReturn` if hero then 0 else 3

  it "Royal Guard searches the deck for Blood Debt and deals it facedown without revealing it" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      [debt,_] <- stackEncounterDeck [C.cw57072,M.hydraMercenary]
      card <- encounterCard C.cw57073
      resolve [RevealEncounterCard you card]
      let sid = SideSchemeId $ unCardId $ ecCardId card
      sideSchemeThreat sid `shouldReturn` 4
      L.runQueueT (L.capture $ L.removeSchemeThreat you (SchemeSideSchemeId sid) 4) >>= resolve
      dealtTo you `shouldReturn` [debt]
      identityMatches (IdentityWithTrait T.Hunted) you `shouldReturn` False
      getEncounterDeck >>= (`shouldSatisfy` notElem debt)

  it "Royal Guard finds Blood Debt in the discard pile too" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      debt <- encounterCard C.cw57072
      resolve [DiscardedCard $ EncounterCard debt]
      card <- encounterCard C.cw57073
      resolve [RevealEncounterCard you card]
      let sid = SideSchemeId $ unCardId $ ecCardId card
      L.runQueueT (L.capture $ L.removeSchemeThreat you (SchemeSideSchemeId sid) 4) >>= resolve
      dealtTo you `shouldReturn` [debt]
      getEncounterDiscard >>= (`shouldSatisfy` notElem debt)

  it "Royal Guard has no choice or replacement effect when Blood Debt is absent" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- stackEncounterDeck [M.hydraMercenary]
      card <- encounterCard C.cw57073
      resolve [RevealEncounterCard you card]
      let sid = SideSchemeId $ unCardId $ ecCardId card
      L.runQueueT (L.capture $ L.removeSchemeThreat you (SchemeSideSchemeId sid) 4) >>= resolve
      dealtTo you `shouldReturn` []

  it "another player's Hunted trait does not activate Janus against someone else" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let one = player1 tg
          two = player2 tg
      _ <- addSecondBoard tg
      debt <- encounterCard C.cw57072
      resolve [RevealEncounterCard two debt]
      cards <- stackDeck one ["01088","01086","01008"]
      _ <- addMinion one C.cw57069
      take 2 <$> discardOf one `shouldReturn` take 2 cards
      damageOf one `shouldReturn` 0

  it "the opposing team chooses Blood Debt for the defeater" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let one = player1 tg
          two = player2 tg
      _ <- addSecondBoard tg
      [debt,_] <- stackEncounterDeck [C.cw57072,M.hydraMercenary]
      card <- encounterCard C.cw57073
      resolve [RevealEncounterCard one card]
      let sid = SideSchemeId $ unCardId $ ecCardId card
      messages <- L.runQueueT $ L.capture $ L.removeSchemeThreat one (SchemeSideSchemeId sid) 4
      start messages
      fmap fst <$> openQuestion `shouldReturn` Just two
      choose "find Blood Debt" (\case CardLabel code _ -> code == "57072"; _ -> False)
      pass
      dealtTo one `shouldReturn` [debt]
      dealtTo two `shouldReturn` []

  it "Janus discards instead when Hunted is present but activation is prohibited" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      debt <- encounterCard C.cw57072
      resolve [RevealEncounterCard you debt]
      mid <- addMinion you C.cw57069
      before <- mainSchemeThreat
      cards <- stackDeck you ["01088","01086","01008"]
      L.runQueueT (L.capture $ L.lastingModifiers GameRef (SourceEntity $ MinionRef mid) [CannotActivate] False) >>= resolve
      repeatCard <- encounterCard C.cw57058
      resolve [RevealEncounterCard you repeatCard]
      mainSchemeThreat `shouldReturn` before
      take 2 <$> discardOf you `shouldReturn` take 2 cards
