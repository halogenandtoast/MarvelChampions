{-# LANGUAGE UndecidableInstances #-}
module Marvel.Scenario where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message
import Marvel.Phase
import Marvel.Queue

data Scenario = TheBreakIn' TheBreakIn | KlawScenario' KlawScenario
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance RunMessage Scenario where
  runMessage = genericRunMessage

lookupScenario :: CardCode -> Maybe Scenario
lookupScenario = flip lookup allScenarios

allScenarios :: HashMap CardCode Scenario
allScenarios = fromList [("01097", TheBreakIn' rhinoScenario)]

newtype TheBreakIn = TheBreakIn ScenarioAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance RunMessage TheBreakIn where
  runMessage msg (TheBreakIn attrs) = TheBreakIn <$> runMessage msg attrs

rhinoScenario :: TheBreakIn
rhinoScenario =
  TheBreakIn $ ScenarioAttrs "01097" ["01094"] (Static 0) (PerPlayer 1) 0

newtype KlawScenario = KlawScenario ScenarioAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance RunMessage KlawScenario where
  runMessage msg (KlawScenario attrs) = KlawScenario <$> runMessage msg attrs

instance Entity Scenario where
  type EntityId Scenario = CardCode
  type EntityAttrs Scenario = ScenarioAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

instance Entity ScenarioAttrs where
  type EntityId ScenarioAttrs = CardCode
  type EntityAttrs ScenarioAttrs = ScenarioAttrs
  toId = scenarioId
  toAttrs = id

data ScenarioAttrs = ScenarioAttrs
  { scenarioId :: CardCode
  , scenarioVillains :: [CardCode]
  , scenarioInitialThreat :: GameValue
  , scenarioAcceleration :: GameValue
  , scenarioThreat :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

threatL :: Lens' ScenarioAttrs Natural
threatL = lens scenarioThreat $ \m x -> m { scenarioThreat = x }

runMainSchemeMessage
  :: MonadGame env m => MainSchemeMessage -> ScenarioAttrs -> m ScenarioAttrs
runMainSchemeMessage msg attrs = case msg of
  MainSchemeThwarted _ n -> pure $ attrs & threatL %~ max 0 . subtract n

instance RunMessage ScenarioAttrs where
  runMessage msg attrs@ScenarioAttrs {..} = case msg of
    StartScenario -> do
      pushAll $ map AddVillain scenarioVillains <> [BeginPhase PlayerPhase]
      pure attrs
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
      pushAll
        [ PlaceThreat
        , VillainAndMinionsActivate
        , DealEncounterCards
        , RevealEncounterCards
        , PassFirstPlayer
        , EndRound
        ]
      pure attrs
    PlaceThreat -> do
      additionalThreat <- fromGameValue scenarioAcceleration
      pure $ attrs & threatL +~ fromIntegral additionalThreat
    EndRound -> do
      push BeginRound
      pure attrs
    BeginRound -> do
      push (BeginPhase PlayerPhase)
      pure attrs
    MainSchemeMessage ident msg' | ident == scenarioId ->
      runMainSchemeMessage msg' attrs
    _ -> pure attrs
