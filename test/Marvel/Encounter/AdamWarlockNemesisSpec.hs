module Marvel.Encounter.AdamWarlockNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (sort)
import Marvel.EncounterSet (EncounterSet (AdamWarlockNemesis), getNemesisSet)
import Marvel.Keyword (Keyword (Incite, Quickstrike, Toughness))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Scenario.Types (Attrs (scenarioSetAsideCards))
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

minionCodes :: GameAppT [CardCode]
minionCodes = getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMinions)

sideSchemeCodes :: GameAppT [CardCode]
sideSchemeCodes = getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameSideSchemes)

setAsideCodes :: GameAppT [CardCode]
setAsideCodes = getsGame (map toCardCode . (.scenarioSetAsideCards) . toAttrs . gameScenario)

deckSize :: IdentityId -> GameAppT Int
deckSize p = length . unDeck . (.playerIdentityDeck) <$> playerAttrs p

-- | The player's deck made exactly new copies of the cards given, the first on top.
setDeck :: IdentityId -> [CardCode] -> GameAppT ()
setDeck p codes = do
  withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDeck = Deck []}
  void $ stackDeck p codes

-- | The Universal Church of Truth in play for the player.
church :: IdentityId -> GameAppT SideSchemeId
church p = do
  scheme <- sideSchemeOf <$> revealNow p SideSchemes.universalChurchOfTruth
  pass
  pure scheme

-- | The minion attacks the player, undefended.
attackedBy :: MinionId -> IdentityId -> GameAppT ()
attackedBy m p = do
  -- its boost card: Hail Hydra! has no boost icons
  _ <- stackEncounterDeck [Treacheries.hailHydra]
  start [MinionMessage m (MinionAttacks p)]
  chooseIf isNoDefender
  chooseIf (isAbilityOf m)
  pass

-- | The player draws their last card, their discard pile not empty: their deck resets.
resetsDeck :: IdentityId -> GameAppT ()
resetsDeck p = do
  setDeck p ["01088"]
  _ <- stackDiscard p ["01089"]
  resolve [IdentityMessage p (DrawCards FromDeck 1)]

spec :: Spec
spec = describe "Adam Warlock's nemesis set" do
  it "is Adam Warlock's nemesis set" do
    getNemesisSet "21031" `shouldBe` AdamWarlockNemesis :: IO ()

  it "is set aside at setup" do
    tg <- testGame ["adam-warlock"]
    inGame tg do
      aside <- setAsideCodes
      for_ ["21067", "21068", "21069", "21070"] \c -> aside `shouldSatisfy` elem c
      liftIO $ length (filter (== "21069") aside) `shouldBe` 2

  it "Shadow of the Past brings out The Magus and the Universal Church of Truth, the rest shuffled in" do
    tg <- testGame ["adam-warlock"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p Treacheries.shadowOfThePast
      pass
      minionCodes >>= (`shouldSatisfy` elem "21067")
      sideSchemeCodes >>= (`shouldSatisfy` elem "21068")
      sort . filter (`elem` ["21069", "21070"]) . map toCardCode <$> scenarioEncounterDeckNow
        `shouldReturn` ["21069", "21069", "21070"]

  describe "The Magus (21067)" do
    it "is unique, with quickstrike and toughness" do
      (liftIO :: IO () -> IO ()) do
        cdUnique Minions.theMagus `shouldBe` True
        cdKeywords Minions.theMagus `shouldSatisfy` \ks -> Quickstrike `elem` ks && Toughness `elem` ks

    it "after he attacks you, discards the top 5 cards of your deck" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        m <- addMinion p Minions.theMagus
        before <- deckSize p
        m `attackedBy` p
        after <- deckSize p
        liftIO $ before - after `shouldBe` 5

    it "after he schemes while engaged with you, discards the top 5 cards of your deck" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        m <- addMinion p Minions.theMagus
        before <- deckSize p
        resolve [MinionMessage m MinionSchemes]
        after <- deckSize p
        liftIO $ before - after `shouldBe` 5

    it "attacking another player discards their deck, not yours" do
      tg <- testGame ["spider-man", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        m <- addMinion p Minions.theMagus
        mine <- deckSize p
        theirs <- deckSize other
        m `attackedBy` other
        deckSize p `shouldReturn` mine
        deckSize other `shouldReturn` (theirs - 5)

    it "emptying your deck with the Church in play resets it: exhausted and stunned" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- church p
        m <- addMinion p Minions.theMagus
        setDeck p ["01088", "01088", "01088"]
        _ <- stackDiscard p ["01089"]
        resolve [MinionMessage m MinionSchemes]
        -- 3 discarded, no more from the deck made again (RR Player Deck)
        deckSize p `shouldReturn` 4
        isStunned p `shouldReturn` True
        isExhaustedNow p `shouldReturn` True

  describe "Universal Church of Truth (21068)" do
    it "enters with 2 threat per player and has an acceleration icon" do
      tg <- testGame ["spider-man", "spider-man"]
      let p = player1 tg
      inGame tg do
        scheme <- church p
        sideSchemeThreat scheme `shouldReturn` 4
        liftIO $ cdAcceleration SideSchemes.universalChurchOfTruth `shouldBe` 1

    it "after a player resets their deck, exhausts and stuns that player only" do
      tg <- testGame ["spider-man", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        _ <- church p
        resetsDeck other
        length <$> dealtTo other `shouldReturn` 1
        isStunned other `shouldReturn` True
        isExhaustedNow other `shouldReturn` True
        isStunned p `shouldReturn` False
        isExhaustedNow p `shouldReturn` False

    it "out of play, a deck reset does nothing" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        resetsDeck p
        length <$> dealtTo p `shouldReturn` 1
        isStunned p `shouldReturn` False
        isExhaustedNow p `shouldReturn` False

    it "as a boost card, is revealed" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- stackEncounterDeck [SideSchemes.universalChurchOfTruth]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        pass
        sideSchemeCodes >>= (`shouldSatisfy` elem "21068")
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "21068")

  describe "Zealot of Truth (21069)" do
    it "keeps threat from being removed from the Church, not from other schemes" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        scheme <- church p
        _ <- addMinion p Minions.zealotOfTruth
        resolve [SideSchemeMessage scheme (SideSchemeThwarted (IdentityRef p) 1)]
        sideSchemeThreat scheme `shouldReturn` 2
        m <- mainSchemeIdNow
        -- threat to remove, the main scheme may start with none
        resolve [MainSchemeMessage m (MainSchemePlaceThreat 2)]
        before <- mainSchemeThreatNow
        resolve [MainSchemeMessage m (MainSchemeThwarted (IdentityRef p) 1)]
        mainSchemeThreatNow `shouldReturn` (before - 1)

    it "once it leaves play, threat can be removed from the Church again" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        scheme <- church p
        z <- addMinion p Minions.zealotOfTruth
        resolve [MinionMessage z MinionDefeated]
        resolve [SideSchemeMessage scheme (SideSchemeThwarted (IdentityRef p) 1)]
        sideSchemeThreat scheme `shouldReturn` 1

    it "as a boost card, is put into play engaged with you" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- stackEncounterDeck [Minions.zealotOfTruth]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        pass
        minionCodes >>= (`shouldSatisfy` elem "21069")

  describe "Cosmic Inquisition (21070)" do
    it "has incite 2" do
      cdKeywords Treacheries.cosmicInquisition `shouldSatisfy` (Incite 2 `elem`) :: IO ()

    it "with the Church in play, discards the top 10 cards of your deck" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- church p
        before <- deckSize p
        threat <- mainSchemeThreatNow
        _ <- revealNow p Treacheries.cosmicInquisition
        pass
        after <- deckSize p
        liftIO $ before - after `shouldBe` 10
        mainSchemeThreatNow `shouldReturn` (threat + 2)

    it "otherwise reveals the Church from the set-aside area" do
      tg <- testGame ["adam-warlock"]
      let p = player1 tg
      inGame tg do
        before <- deckSize p
        _ <- revealNow p Treacheries.cosmicInquisition
        pass
        sideSchemeCodes >>= (`shouldSatisfy` elem "21068")
        setAsideCodes >>= (`shouldSatisfy` notElem "21068")
        deckSize p `shouldReturn` before

    it "otherwise reveals the Church from the encounter discard pile" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- setEncounterDeck [Treacheries.hailHydra] [SideSchemes.universalChurchOfTruth]
        _ <- revealNow p Treacheries.cosmicInquisition
        pass
        sideSchemeCodes >>= (`shouldSatisfy` elem "21068")
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "21068")
