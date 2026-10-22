module Marvel.Scenario.Types where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Data.Function (on)
import Data.List qualified as List
import Data.Typeable
import Marvel.Card
import Marvel.Choice (chooseOrRunOne)
import Marvel.Difficulty
import Marvel.EncounterCard
import Marvel.EncounterSet
import Marvel.EncounterSet qualified as EncounterSet
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message
import Marvel.Identity.Types (Field (PlayerIdentityEncounterCards), PlayerIdentity)
import Marvel.Phase
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Window qualified as W
import System.Random.Shuffle

data Scenario = forall a. (IsScenario a) => Scenario a

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, RunMessage a) => IsScenario a where
  toScenarioAttrs :: a -> Attrs Scenario
  default toScenarioAttrs :: (Coercible a (Attrs Scenario)) => a -> Attrs Scenario
  toScenarioAttrs = coerce

instance Show Scenario where
  show (Scenario a) = show a

instance ToJSON Scenario where
  toJSON (Scenario a) = toJSON a

instance Eq Scenario where
  (Scenario (a :: a)) == (Scenario (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeScenarioCard
  = forall a.
    (IsScenario a) =>
    SomeScenarioCard
      (ScenarioCard a)

liftScenarioCard :: (forall a. ScenarioCard a -> b) -> SomeScenarioCard -> b
liftScenarioCard f (SomeScenarioCard a) = f a

someScenarioCardCode :: SomeScenarioCard -> CardCode
someScenarioCardCode = liftScenarioCard scenarioCardCode

instance RunMessage Scenario where
  runMessage msg (Scenario a) = Scenario <$> runMessage msg a

-- | How a scenario is to be played: the villain's, with which modular sets and at which
-- difficulty. Expert adds the expert set and starts the villain a stage later.
data ScenarioSetup = ScenarioSetup
  { scenarioCode :: CardCode
  , modulars :: [EncounterSet]
  , difficulty :: Difficulty
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ScenarioCard a = ScenarioCard
  { scenarioCardCode :: CardCode
  , buildScenario :: ScenarioSetup -> a
  }

instance Entity Scenario where
  type Id Scenario = CardCode
  data Attrs Scenario = ScenarioAttrs
    { scenarioId :: CardCode
    , scenarioVillains :: [CardCode]
    , scenarioMainSchemes :: [CardCode]
    , scenarioEncounterSets :: HashSet EncounterSet
    , scenarioEncounterDeck :: [EncounterCard]
    , scenarioDiscard :: [EncounterCard]
    , scenarioDifficulty :: Difficulty
    , scenarioSetAsideCards :: [Card]
    , scenarioAccelerationTokens :: Natural
    , scenarioReshufflesThisRound :: Natural
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
  data Field Scenario :: Type -> Type where
    ScenarioId :: Field Scenario CardCode
    ScenarioVillains :: Field Scenario [CardCode]
    ScenarioMainSchemes :: Field Scenario [CardCode]
    ScenarioEncounterSets :: Field Scenario (HashSet EncounterSet)
    ScenarioEncounterDeck :: Field Scenario [EncounterCard]
    ScenarioDiscard :: Field Scenario [EncounterCard]
    ScenarioDifficulty :: Field Scenario Difficulty
    ScenarioSetAsideCards :: Field Scenario [Card]
    ScenarioAccelerationTokens :: Field Scenario Natural
    ScenarioReshufflesThisRound :: Field Scenario Natural
  field fld s =
    let ScenarioAttrs {..} = toAttrs s
     in case fld of
          ScenarioId -> scenarioId
          ScenarioVillains -> scenarioVillains
          ScenarioMainSchemes -> scenarioMainSchemes
          ScenarioEncounterSets -> scenarioEncounterSets
          ScenarioEncounterDeck -> scenarioEncounterDeck
          ScenarioDiscard -> scenarioDiscard
          ScenarioDifficulty -> scenarioDifficulty
          ScenarioSetAsideCards -> scenarioSetAsideCards
          ScenarioAccelerationTokens -> scenarioAccelerationTokens
          ScenarioReshufflesThisRound -> scenarioReshufflesThisRound
  toId = scenarioId . toAttrs
  toAttrs (Scenario a) = toScenarioAttrs a

getScenarioDifficulty :: Scenario -> Difficulty
getScenarioDifficulty = scenarioDifficulty . toAttrs

discardL :: Lens' (Attrs Scenario) [EncounterCard]
discardL = lens scenarioDiscard $ \m x -> m {scenarioDiscard = x}

encounterDeckL :: Lens' (Attrs Scenario) [EncounterCard]
encounterDeckL =
  lens scenarioEncounterDeck $ \m x -> m {scenarioEncounterDeck = x}

setAsideCardsL :: Lens' (Attrs Scenario) [Card]
setAsideCardsL =
  lens scenarioSetAsideCards $ \m x -> m {scenarioSetAsideCards = x}

accelerationTokensL :: Lens' (Attrs Scenario) Natural
accelerationTokensL =
  lens scenarioAccelerationTokens $ \m x -> m {scenarioAccelerationTokens = x}

mainSchemesL :: Lens' (Attrs Scenario) [CardCode]
mainSchemesL = lens scenarioMainSchemes $ \m x -> m {scenarioMainSchemes = x}

-- | A scenario: its villain's stage for standard and for expert play, its main schemes
-- in order, and its own encounter sets, to which the setup adds the modular sets, the
-- standard set and, for expert play, the expert set.
scenario ::
  (Attrs Scenario -> a) ->
  CardCode ->
  (CardCode, CardCode) ->
  [CardCode] ->
  [EncounterSet] ->
  ScenarioCard a
scenario f cCode (standardVillain, expertVillain) mainSchemes encounterSets =
  ScenarioCard
    { scenarioCardCode = cCode
    , buildScenario = \setup ->
        f $
          ScenarioAttrs
            { scenarioId = cCode
            , scenarioVillains = [if setup.difficulty == Expert then expertVillain else standardVillain]
            , scenarioMainSchemes = mainSchemes
            , scenarioEncounterDeck = mempty
            , scenarioDiscard = mempty
            , scenarioDifficulty = setup.difficulty
            , scenarioEncounterSets =
                HashSet.fromList $
                  encounterSets
                    <> setup.modulars
                    <> [EncounterSet.Standard]
                    <> [EncounterSet.ExpertSet | setup.difficulty == Expert]
            , scenarioSetAsideCards = mempty
            , scenarioAccelerationTokens = 0
            , scenarioReshufflesThisRound = 0
            }
    }

-- | However it empties, the encounter deck is made again from the discard pile at once,
-- with an acceleration token; with no discard pile to make it from, the tokens would be
-- placed without end, and the players lose.
instance RunMessage (Attrs Scenario) where
  runMessage msg attrs = do
    attrs' <- runScenarioMessage msg attrs
    case (scenarioEncounterDeck attrs', scenarioDiscard attrs') of
      ([], []) | notNull (scenarioEncounterDeck attrs) -> attrs' <$ push (GameOver Lost)
      ([], discard) | notNull discard -> do
        deck' <- shuffleM discard
        pure $
          attrs'
            & (accelerationTokensL +~ 1)
            & (encounterDeckL .~ deck')
            & (discardL .~ mempty)
            & \a -> a {scenarioReshufflesThisRound = scenarioReshufflesThisRound a + 1}
      _ -> pure attrs'

-- | The top card of the encounter deck, which is never empty while its discard pile has
-- cards: with neither, the players have lost.
drawEncounterCard :: (HasQueue m) => Attrs Scenario -> m ([EncounterCard], Attrs Scenario)
drawEncounterCard attrs = case scenarioEncounterDeck attrs of
  [] -> ([], attrs) <$ push (GameOver Lost)
  card : deck' -> pure ([card], attrs & encounterDeckL .~ deck')

-- | The card leaves the encounter deck, discard pile or set-aside area, wherever it is.
leaves :: EncounterCard -> Attrs Scenario -> Attrs Scenario
leaves card attrs =
  attrs
    & (setAsideCardsL %~ filter (/= EncounterCard card))
    & (encounterDeckL %~ filter (/= card))
    & (discardL %~ filter (/= card))

runScenarioMessage :: (HasCallStack, MonadThrow m, MonadRandom m, HasGame m, HasQueue m, Projection m PlayerIdentity) => Message -> Attrs Scenario -> m (Attrs Scenario)
runScenarioMessage msg attrs@ScenarioAttrs {..} = case msg of
    StartScenario -> do
      encounterCards <-
        shuffleM
          . (<> scenarioEncounterDeck)
          =<< gatherEncounterSets scenarioEncounterSets
      let
        (mainScheme, remainingMainSchemes) = case scenarioMainSchemes of
          [] -> error "No main schemes"
          x : xs -> (x, xs)
      pushAll $
        map AddVillain scenarioVillains
          <> [AddSetupMainScheme mainScheme]
      pure $
        attrs
          & encounterDeckL
            .~ encounterCards
          & mainSchemesL
            .~ remainingMainSchemes
    NextMainScheme -> do
      let
        (mainScheme, remainingMainSchemes) = case scenarioMainSchemes of
          [] -> error "No main schemes"
          x : xs -> (x, xs)
      push (ReplaceMainScheme mainScheme)
      pure $ attrs & mainSchemesL .~ remainingMainSchemes
    ShuffleEncounterDeck -> do
      deck' <- shuffleM scenarioEncounterDeck
      pure $ attrs & encounterDeckL .~ deck'
    ShuffleIntoEncounterDeck cards -> do
      deck' <- shuffleM (scenarioEncounterDeck <> cards)
      pure $ attrs & encounterDeckL .~ deck' & setAsideCardsL %~ filter (`notElem` map EncounterCard cards)
    -- a card comes out from wherever it was: the set-aside area (a nemesis set's), or
    -- the encounter deck or discard pile searched
    RevealEncounterCard _ card -> pure $ leaves card attrs
    PutEncounterCardIntoPlay _ card -> pure $ leaves card attrs
    BeginPhase PlayerPhase -> do
      players <- getPlayers
      pushAll $
        concatMap (\p -> [SetActivePlayer p, IdentityMessage p BeginTurn]) players
          <> [EndPhase PlayerPhase]
      pure attrs
    EndPhase PlayerPhase -> do
      players <- getPlayers
      pushAll $
        concatMap
          (\ident -> [SetActivePlayer ident, IdentityMessage ident DiscardCards, IdentityMessage ident DrawOrDiscardToHandLimit])
          players
          <> map (($ ReadyCards) . IdentityMessage) players
          <> map SetActivePlayer (take 1 players)
          <> [BeginPhase VillainPhase]
      pure attrs
    AddAccelerationToken -> do
      pure $ attrs & accelerationTokensL +~ 1
    BeginPhase VillainPhase -> do
      players <- getPlayers
      hazards <- fromIntegral <$> getHazardCount
      pushAll $
        AccelerateMainScheme
          : concatMap (\p -> [SetActivePlayer p, IdentityMessage p VillainAndMinionsActivate]) players
            <> map DealEncounterCard players
            <> zipWith ($) (replicate hazards DealEncounterCard) (cycle players)
            <> [RevealDealtEncounterCards, PassFirstPlayer, EndRound]
      pure attrs
    -- each player in turn reveals the cards dealt them, until none remain
    RevealDealtEncounterCards -> do
      players <- filterM (fmap notNull . project PlayerIdentityEncounterCards) =<< getPlayers
      unless (null players) $
        pushAll $
          concatMap (\p -> [SetActivePlayer p, IdentityMessage p RevealEncounterCards]) players
            <> [RevealDealtEncounterCards]
      pure attrs
    EndRound -> do
      pushAll [CheckWindows [W.Window W.When W.RoundEnded], BeginRound]
      pure attrs {scenarioReshufflesThisRound = 0}
    BeginRound -> do
      push (BeginPhase PlayerPhase)
      pure attrs
    DealEncounterCard ident -> do
      (ecs, attrs') <- drawEncounterCard attrs
      pushAll $ map (IdentityMessage ident . DealtEncounterCard) ecs
      pure attrs'
    -- ponytail: a deck worn down to surge cards would surge forever, so after a few
    -- reshuffles in a round surge stops; a real table would just stop too
    Surge _ | scenarioReshufflesThisRound >= 3 -> pure attrs
    -- the surge keyword: "When Revealed: Deal yourself 1 facedown encounter card"
    Surge ident -> do
      pushAll [CheckWindows [W.Window W.Would $ W.SurgeResolves ident], DealEncounterCard ident]
      pure attrs
    DrawAndRevealEncounterCard ident -> do
      (ecs, attrs') <- drawEncounterCard attrs
      pushAll $ map (RevealEncounterCard ident) ecs
      pure attrs'
    DealBoost target -> do
      (ecs, attrs') <- drawEncounterCard attrs
      case target of
        VillainRef vid ->
          pushAll $ map (VillainMessage vid . VillainDealtBoost) ecs
        _ -> error "Can not deal boost to target"
      pure attrs'
    -- a deck emptied partway is fulfilled: the new deck is not discarded from
    DiscardTopOfEncounterDeck n mTarget -> do
      let (discarded, deck') = splitAt (fromIntegral n) scenarioEncounterDeck
      pushAll [WithDiscarded target FromEncounterDeck (EncounterCard <$> discarded) | Just target <- [mTarget]]
      pure $ attrs & encounterDeckL .~ deck' & discardL %~ (reverse discarded <>)
    SetAside cards -> pure $ attrs & setAsideCardsL <>~ cards
    -- a card discarded from the encounter deck, as one looked at there, leaves it
    DiscardedCard (EncounterCard ec) -> pure $ attrs & discardL %~ (ec :) & encounterDeckL %~ filter (/= ec)
    PutOnTopOfEncounterDeck cards -> pure $ attrs & encounterDeckL %~ (cards <>) . filter (`notElem` cards)
    LookAtTopOfEncounterDeck n target -> do
      push $ SearchFoundCards target $ EncounterCard <$> take (fromIntegral n) scenarioEncounterDeck
      pure attrs
    DiscardUntil FromEncounterDeck cardMatcher target -> do
      let
        (discarded, remaining) =
          break (cardMatch cardMatcher) scenarioEncounterDeck
      case remaining of
        [] -> pure $ attrs & encounterDeckL .~ [] & discardL %~ (reverse discarded <>)
        x : xs -> do
          push $ WithDiscardedMatch target FromEncounterDeck (EncounterCard x)
          pure $ attrs & encounterDeckL .~ xs & discardL %~ (reverse discarded <>)
    SearchForAndRevealScheme cardDef -> do
      ident <- selectJust You
      search ident (CardIs cardDef) (RevealEncounterCard ident)
    SearchForAndRevealEncounterCard ident matcher -> search ident matcher (RevealEncounterCard ident)
    SearchForAndPutIntoPlayEncounterCard ident matcher -> search ident matcher (PutEncounterCardIntoPlay ident)
    _ -> pure attrs
 where
  -- the searching player chooses one of the cards found, then the deck is shuffled
  search ident matcher f = do
    let found = List.nubBy ((==) `on` toCardCode) $ filter (cardMatch matcher) (scenarioEncounterDeck <> scenarioDiscard)
    push ShuffleEncounterDeck
    unless (null found) $ chooseOrRunOne ident [TargetLabel (toRef card) [Run [f card]] | card <- found]
    pure attrs
