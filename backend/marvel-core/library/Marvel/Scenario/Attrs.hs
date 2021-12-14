{-# LANGUAGE TemplateHaskell #-}

module Marvel.Scenario.Attrs where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Marvel.Card
import Marvel.Difficulty
import Marvel.EncounterCard
import Marvel.EncounterSet
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message
import Marvel.Phase
import Marvel.Query
import Marvel.Queue
import Marvel.Target
import Marvel.Window qualified as W
import System.Random.Shuffle

data ScenarioAttrs = ScenarioAttrs
  { scenarioId :: CardCode
  , scenarioVillains :: [CardCode]
  , scenarioMainSchemes :: [CardCode]
  , scenarioEncounterSets :: HashSet EncounterSet
  , scenarioEncounterDeck :: [EncounterCard]
  , scenarioDiscard :: [EncounterCard]
  , scenarioDifficulty :: Difficulty
  , scenarioSetAsideCards :: [Card]
  , scenarioAccelerationTokens :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

scenario ::
  (ScenarioAttrs -> a) ->
  CardCode ->
  [CardCode] ->
  [CardCode] ->
  [EncounterSet] ->
  a
scenario f cCode villains mainSchemes encounterSets =
  f $
    ScenarioAttrs
      { scenarioId = cCode
      , scenarioVillains = villains
      , scenarioMainSchemes = mainSchemes
      , scenarioEncounterDeck = mempty
      , scenarioDiscard = mempty
      , scenarioDifficulty = Normal
      , scenarioEncounterSets = HashSet.fromList encounterSets
      , scenarioSetAsideCards = mempty
      , scenarioAccelerationTokens = 0
      }

makeLensesWith suffixedFields ''ScenarioAttrs

instance RunMessage ScenarioAttrs where
  runMessage msg attrs@ScenarioAttrs {..} = case msg of
    StartScenario -> do
      encounterCards <-
        shuffleM
          . (<> scenarioEncounterDeck)
          =<< gatherEncounterSets scenarioEncounterSets
      let (mainScheme, remainingMainSchemes) = case scenarioMainSchemes of
            [] -> error "No main schemes"
            x : xs -> (x, xs)
      pushAll $
        map AddVillain scenarioVillains
          <> [AddMainScheme mainScheme, SetupMainScheme, BeginPhase PlayerPhase]
      pure $ attrs & encounterDeckL .~ encounterCards & mainSchemesL .~ remainingMainSchemes
    NextMainScheme -> do
      let (mainScheme, remainingMainSchemes) = case scenarioMainSchemes of
            [] -> error "No main schemes"
            x : xs -> (x, xs)
      push (ReplaceMainScheme mainScheme)
      pure $ attrs & mainSchemesL .~ remainingMainSchemes
    ShuffleEncounterDeck -> do
      deck' <- shuffleM scenarioEncounterDeck
      pure $ attrs & encounterDeckL .~ deck'
    ShuffleIntoEncounterDeck cards -> do
      deck' <- shuffleM (scenarioEncounterDeck <> cards)
      pure $ attrs & encounterDeckL .~ deck'
    BeginPhase PlayerPhase -> do
      players <- getPlayers
      pushAll $
        map (($ BeginTurn) . IdentityMessage) players
          <> [EndPhase PlayerPhase]
      pure attrs
    EndPhase PlayerPhase -> do
      players <- getPlayers
      pushAll $
        concatMap
          ( \ident ->
              map
                (IdentityMessage ident)
                [DiscardCards, DrawOrDiscardToHandLimit]
          )
          players
          <> map (($ ReadyCards) . IdentityMessage) players
          <> [BeginPhase VillainPhase]
      pure attrs
    AddAccelerationToken -> do
      pure $ attrs & accelerationTokensL +~ 1
    BeginPhase VillainPhase -> do
      players <- getPlayers
      hazards <- fromIntegral <$> getHazardCount
      pushAll $
        AccelerateMainScheme :
        map (($ VillainAndMinionsActivate) . IdentityMessage) players
          <> map DealEncounterCard players
          <> zipWith ($) (replicate hazards DealEncounterCard) (cycle players)
          <> map (($ RevealEncounterCards) . IdentityMessage) players
          <> [PassFirstPlayer, EndRound]
      pure attrs
    EndRound -> do
      pushAll [CheckWindows [W.Window W.When W.RoundEnded], BeginRound]
      pure attrs
    BeginRound -> do
      push (BeginPhase PlayerPhase)
      pure attrs
    EmptyScenarioDeck -> do
      deck' <- shuffleM scenarioDiscard
      pure $
        attrs
          & (accelerationTokensL +~ 1)
          & (encounterDeckL .~ deck')
          & (discardL .~ mempty)
    DealEncounterCard ident -> do
      let (ecs, deck') = splitAt 1 scenarioEncounterDeck
      pushAll $ map (IdentityMessage ident . DealtEncounterCard) ecs
      when (null deck') (push EmptyScenarioDeck)
      pure $ attrs & encounterDeckL .~ deck'
    Surge ident -> do
      let (ecs, deck') = splitAt 1 scenarioEncounterDeck
      pushAll $ map (RevealEncounterCard ident) ecs
      when (null deck') (push EmptyScenarioDeck)
      pure $ attrs & encounterDeckL .~ deck'
    DrawAndRevealEncounterCard ident -> do
      let (ecs, deck') = splitAt 1 scenarioEncounterDeck
      pushAll $ map (RevealEncounterCard ident) ecs
      when (null deck') (push EmptyScenarioDeck)
      pure $ attrs & encounterDeckL .~ deck'
    DealBoost target -> do
      let (ecs, deck') = splitAt 1 scenarioEncounterDeck
      case target of
        VillainTarget vid ->
          pushAll $ map (VillainMessage vid . VillainDealtBoost) ecs
        _ -> error "Can not deal boost to target"
      when (null deck') (push EmptyScenarioDeck)
      pure $ attrs & encounterDeckL .~ deck'
    SetAside cards -> pure $ attrs & setAsideCardsL <>~ cards
    DiscardedCard (EncounterCard ec) -> pure $ attrs & discardL %~ (ec :)
    SearchForAndRevealScheme cardDef -> do
      ident <- selectJust You
      case find ((== cardDef) . getCardDef) $
        scenarioEncounterDeck
          <> scenarioDiscard of
        Nothing -> do
          push ShuffleEncounterDeck
          pure attrs
        Just card -> do
          pushAll [RevealEncounterCard ident card, ShuffleEncounterDeck]
          pure $
            attrs
              & (discardL %~ filter (/= card))
              & (encounterDeckL %~ filter (/= card))
    _ -> pure attrs

instance Entity ScenarioAttrs where
  type EntityId ScenarioAttrs = CardCode
  type EntityAttrs ScenarioAttrs = ScenarioAttrs
  toId = scenarioId
  toAttrs = id
