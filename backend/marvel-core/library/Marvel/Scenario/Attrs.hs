{-# LANGUAGE TemplateHaskell #-}
module Marvel.Scenario.Attrs where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.EncounterCard
import Marvel.Difficulty
import Marvel.EncounterCard
import Marvel.EncounterSet
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
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
  , scenarioInitialThreat :: GameValue
  , scenarioAcceleration :: GameValue
  , scenarioAccelerationTokens :: Natural
  , scenarioThreat :: Natural
  , scenarioThreshold :: GameValue
  , scenarioEncounterSets :: HashSet EncounterSet
  , scenarioEncounterDeck :: [EncounterCard]
  , scenarioDiscard :: [EncounterCard]
  , scenarioDifficulty :: Difficulty
  , scenarioSetAsideCards :: [EncounterCard]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

scenario
  :: (ScenarioAttrs -> a)
  -> CardCode
  -> [CardCode]
  -> [EncounterSet]
  -> GameValue
  -> GameValue
  -> GameValue
  -> a
scenario f cCode villains encounterSets threshold iThreat acceleration =
  f $ ScenarioAttrs
    { scenarioId = cCode
    , scenarioVillains = villains
    , scenarioInitialThreat = iThreat
    , scenarioAcceleration = acceleration
    , scenarioAccelerationTokens = 0
    , scenarioThreat = 0
    , scenarioEncounterDeck = mempty
    , scenarioDiscard = mempty
    , scenarioDifficulty = Normal
    , scenarioEncounterSets = HashSet.fromList encounterSets
    , scenarioThreshold = threshold
    , scenarioSetAsideCards = mempty
    }

makeLensesWith suffixedFields ''ScenarioAttrs

runMainSchemeMessage
  :: MonadGame env m => MainSchemeMessage -> ScenarioAttrs -> m ScenarioAttrs
runMainSchemeMessage msg attrs = case msg of
  MainSchemeThwarted _ n -> pure $ attrs & threatL %~ subtractNatural n
  MainSchemePlaceThreat n -> do
    threshold <- fromIntegral <$> fromGameValue (scenarioThreshold attrs)
    when (scenarioThreat attrs + n >= threshold) (push AdvanceScenario)
    pure $ attrs & threatL +~ n

instance RunMessage ScenarioAttrs where
  runMessage msg attrs@ScenarioAttrs {..} = case msg of
    StartScenario -> do
      players <- getPlayers
      encounterCards <-
        shuffleM
        . (<> scenarioEncounterDeck)
        =<< gatherEncounterSets scenarioEncounterSets
      pushAll
        $ map AddVillain scenarioVillains
        <> concatMap
             (\ident -> map
               (IdentityMessage ident)
               [DiscardCards, DrawOrDiscardToHandLimit]
             )
             players
        <> [BeginPhase PlayerPhase]
      pure $ attrs & encounterDeckL .~ encounterCards
    ShuffleEncounterDeck -> do
      deck' <- shuffleM scenarioEncounterDeck
      pure $ attrs & encounterDeckL .~ deck'
    ShuffleIntoEncounterDeck cards -> do
      deck' <- shuffleM (scenarioEncounterDeck <> cards)
      pure $ attrs & encounterDeckL .~ deck'
    BeginPhase PlayerPhase -> do
      players <- getPlayers
      pushAll
        $ map (($ BeginTurn) . IdentityMessage) players
        <> [EndPhase PlayerPhase]
      pure attrs
    EndPhase PlayerPhase -> do
      players <- getPlayers
      pushAll
        $ concatMap
            (\ident -> map
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
      acceleration <- getAccelerationCount
      additionalThreat <- fromIntegral <$> fromGameValue scenarioAcceleration
      hazards <- fromIntegral <$> getHazardCount
      pushAll
        $ CheckWindows
            [ W.Window W.Would
              $ W.ThreatPlaced (SchemeMainSchemeId scenarioId)
              $ additionalThreat
              + acceleration
              + scenarioAccelerationTokens
            ]
        : MainSchemeMessage
            scenarioId
            (MainSchemePlaceThreat $ additionalThreat + acceleration)
        : map (($ VillainAndMinionsActivate) . IdentityMessage) players
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
      pure
        $ attrs
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
    DealBoost target -> do
      let (ecs, deck') = splitAt 1 scenarioEncounterDeck
      case target of
        VillainTarget vid ->
          pushAll
            $ map (`RevealBoostCard` EnemyVillainId vid) ecs
            <> map (VillainMessage vid . VillainDealtBoost) ecs
        _ -> error "Can not deal boost to target"
      when (null deck') (push EmptyScenarioDeck)
      pure $ attrs & encounterDeckL .~ deck'
    SetAside cards -> pure $ attrs & setAsideCardsL <>~ cards
    DiscardedEncounterCard ec -> pure $ attrs & discardL %~ (ec :)
    MainSchemeMessage ident msg' | ident == scenarioId ->
      runMainSchemeMessage msg' attrs
    SearchForAndRevealScheme cardDef -> do
      ident <- selectJust You
      case
          find ((== cardDef) . getCardDef)
          $ scenarioEncounterDeck
          <> scenarioDiscard
        of
          Nothing -> pure attrs
          Just card -> do
            push $ RevealEncounterCard ident card
            pure
              $ attrs
              & discardL
              %~ filter (/= card)
              & encounterDeckL
              %~ filter (/= card)
    _ -> pure attrs

instance Entity ScenarioAttrs where
  type EntityId ScenarioAttrs = CardCode
  type EntityAttrs ScenarioAttrs = ScenarioAttrs
  toId = scenarioId
  toAttrs = id
