module Marvel.Scenario.Attrs where

import Marvel.Prelude

import qualified Data.HashSet as HashSet
import Marvel.Card.Code
import Marvel.Card.EncounterCard
import Marvel.EncounterCard
import Marvel.EncounterSet
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message
import Marvel.Phase
import Marvel.Queue
import Marvel.Target
import System.Random.Shuffle

data ScenarioAttrs = ScenarioAttrs
  { scenarioId :: CardCode
  , scenarioVillains :: [CardCode]
  , scenarioInitialThreat :: GameValue
  , scenarioAcceleration :: GameValue
  , scenarioThreat :: Natural
  , scenarioEncounterSets :: HashSet EncounterSet
  , scenarioEncounterDeck :: [EncounterCard]
  , scenarioDiscard :: [EncounterCard]
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
  -> a
scenario f cCode villains encounterSets iThreat acceleration =
  f $ ScenarioAttrs
    { scenarioId = cCode
    , scenarioVillains = villains
    , scenarioInitialThreat = iThreat
    , scenarioAcceleration = acceleration
    , scenarioThreat = 0
    , scenarioEncounterDeck = mempty
    , scenarioDiscard = mempty
    , scenarioEncounterSets = HashSet.fromList encounterSets
    }

threatL :: Lens' ScenarioAttrs Natural
threatL = lens scenarioThreat $ \m x -> m { scenarioThreat = x }

encounterDeckL :: Lens' ScenarioAttrs [EncounterCard]
encounterDeckL =
  lens scenarioEncounterDeck $ \m x -> m { scenarioEncounterDeck = x }

discardL :: Lens' ScenarioAttrs [EncounterCard]
discardL = lens scenarioDiscard $ \m x -> m { scenarioDiscard = x }

runMainSchemeMessage
  :: MonadGame env m => MainSchemeMessage -> ScenarioAttrs -> m ScenarioAttrs
runMainSchemeMessage msg attrs = case msg of
  MainSchemeThwarted _ n -> pure $ attrs & threatL %~ max 0 . subtract n
  MainSchemePlaceThreat n -> pure $ attrs & threatL +~ n

instance RunMessage ScenarioAttrs where
  runMessage msg attrs@ScenarioAttrs {..} = case msg of
    StartScenario -> do
      encounterCards <- shuffleM =<< gatherEncounterSets scenarioEncounterSets
      pushAll $ map AddVillain scenarioVillains <> [BeginPhase PlayerPhase]
      pure $ attrs & encounterDeckL .~ encounterCards
    BeginPhase PlayerPhase -> do
      players <- getPlayers
      pushAll
        $ map (($ BeginTurn) . IdentityMessage) players
        <> [EndPhase PlayerPhase]
      pure attrs
    EndPhase PlayerPhase -> do
      players <- getPlayers
      pushAll
        $ map (($ DrawOrDiscardToHandLimit) . IdentityMessage) players
        <> map (($ ReadyCards) . IdentityMessage) players
        <> [BeginPhase VillainPhase]
      pure attrs
    BeginPhase VillainPhase -> do
      players <- getPlayers
      additionalThreat <- fromIntegral <$> fromGameValue scenarioAcceleration
      pushAll
        $ MainSchemeMessage scenarioId (MainSchemePlaceThreat additionalThreat)
        : map (($ VillainAndMinionsActivate) . IdentityMessage) players
        <> map DealEncounterCard players
        <> map (($ RevealEncounterCards) . IdentityMessage) players
        <> [PassFirstPlayer, EndRound]
      pure attrs
    EndRound -> do
      push BeginRound
      pure attrs
    BeginRound -> do
      push (BeginPhase PlayerPhase)
      pure attrs
    DealEncounterCard ident -> do
      let (ecs, deck') = splitAt 1 scenarioEncounterDeck
      pushAll $ map (IdentityMessage ident . DealtEncounterCard) ecs
      pure $ attrs & encounterDeckL .~ deck'
    DealBoost target -> do
      let (ecs, deck') = splitAt 1 scenarioEncounterDeck
      case target of
        VillainTarget vid ->
          pushAll $ map (VillainMessage vid . DealtBoost) ecs
        _ -> error "Can not deal boost to target"
      pure $ attrs & encounterDeckL .~ deck'
    DiscardedEncounterCard ec -> pure $ attrs & discardL %~ (ec :)
    MainSchemeMessage ident msg' | ident == scenarioId ->
      runMainSchemeMessage msg' attrs
    _ -> pure attrs

instance Entity ScenarioAttrs where
  type EntityId ScenarioAttrs = CardCode
  type EntityAttrs ScenarioAttrs = ScenarioAttrs
  toId = scenarioId
  toAttrs = id