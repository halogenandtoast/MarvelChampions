{-# LANGUAGE PatternSynonyms #-}

module Marvel.Encounter.KeywordsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Marvel.Matchers
    ( SchemeMatcher(ThwartableScheme), data AttackableEnemy )
import Marvel.Minion.Cards qualified as Cards
import Marvel.Query (select)
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types (villainIsTough)
import TestImport

rhinoWith :: [Text] -> IO GameApp
rhinoWith heroes = do
  let setup = fromMaybe (error "no setup") (recommendedSetup "01094")
  (app, _) <- dealtGame setup (map starter heroes) 1
  runGameApp app toFirstTurn
  pure app

villainEnemy :: GameAppT EnemyId
villainEnemy = getsGame (HashMap.keys . gameVillains) >>= \case
  [vid] -> pure (EnemyVillainId vid)
  _ -> error "expected one villain"

spec :: Spec
spec = describe "encounter keywords and icons" do
  it "a guard minion keeps the player it is engaged with, and only them, from attacking the villain" do
    app <- rhinoWith ["spider-man", "captain-marvel"]
    runGameApp app do
      villain <- villainEnemy
      ps <- players
      let (you, other) = case ps of
            [a, b] -> (a, b)
            _ -> error "two players"
      resolve [SetActivePlayer you]
      (liftIO . (`shouldSatisfy` HashSet.member villain)) =<< select AttackableEnemy
      mercenary <- encounterCard Cards.hydraMercenary
      resolve [RevealEncounterCard other mercenary, SetActivePlayer you]
      (liftIO . (`shouldSatisfy` HashSet.member villain)) =<< select AttackableEnemy
      resolve [SetActivePlayer other]
      (liftIO . (`shouldNotSatisfy` HashSet.member villain)) =<< select AttackableEnemy

  it "a crisis side scheme keeps the main scheme from being thwarted" do
    app <- rhinoWith ["spider-man"]
    runGameApp app do
      mainScheme <- getsGame (HashMap.keys . gameMainSchemes) >>= \case
        [m] -> pure m
        _ -> error "expected one main scheme"
      resolve [MainSchemeMessage mainScheme (MainSchemePlaceThreat 2)]
      (liftIO . (`shouldSatisfy` HashSet.member (SchemeMainSchemeId mainScheme))) =<< select ThwartableScheme
      [you] <- players
      crowdControl <- encounterCard Cards.crowdControl
      resolve [RevealEncounterCard you crowdControl]
      thwartable <- select ThwartableScheme
      liftIO $ thwartable `shouldNotSatisfy` HashSet.member (SchemeMainSchemeId mainScheme)
      liftIO $ thwartable `shouldNotSatisfy` null

  it "a boost star ability resolves when the card is turned up as a boost" do
    app <- rhinoWith ["spider-man"]
    runGameApp app do
      villain <- villainEnemy
      tigerShark <- encounterCard Cards.tigerShark
      resolve [RevealBoostCard tigerShark villain, ClearBoosts]
      tough <- getsGame (map villainIsTough . HashMap.elems . gameVillains)
      liftIO $ tough `shouldBe` [True]

  -- RR v1.8 Surge: "When Revealed: Deal yourself 1 facedown encounter card", dealt before
  -- the rest of the card resolves; the dealt card is revealed afterwards (issue #31)
  it "surge deals its card before the card's own When Revealed: Under Fire reveals the next card" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      -- a card left below, so the deck does not run out
      (dealt : revealed : _, _) <- setEncounterDeck [Treacheries.advance, Cards.hydraMercenary, Treacheries.advance] []
      underFire <- encounterCard Treacheries.underFire
      start [RevealEncounterCard you underFire]
      map ecCardId <$> dealtTo you `shouldReturn` [ecCardId dealt]
      getsGame (HashMap.member (minionOf revealed) . gameMinions) `shouldReturn` True

  it "a surge minion deals its card before its When Revealed resolves" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      convict <- encounterCard Cards.escapedConvict
      start [RevealEncounterCard you convict]
      length <$> dealtTo you `shouldReturn` 1
    -- latest first
    ran <- reverse <$> readIORef tg.ran
    let firstIndex p = headMay [i | (i, m) <- zip [0 :: Int ..] ran, p m]
        revealMinion m = "MinionMessage " `T.isPrefixOf` m && "RevealMinion" `T.isInfixOf` m
    liftIO $ ((<) <$> firstIndex ("Surge " `T.isPrefixOf`) <*> firstIndex revealMinion) `shouldBe` Just True

  it "surge a treachery gains as it resolves still deals one card, once it has resolved" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      v <- villainIdNow
      resolve [GiveStatus (VillainRef v) ToughStatus]
      imTough <- encounterCard Treacheries.imTough
      start [RevealEncounterCard you imTough]
      length <$> dealtTo you `shouldReturn` 1
