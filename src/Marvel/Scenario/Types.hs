module Marvel.Scenario.Types where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Function (on)
import Data.List qualified as List
import Data.Typeable
import Marvel.Area (discardsTo, emptyPile, isEncounterDeck)
import Marvel.Campaign.Steps (runCampaignStep)
import Marvel.Card
import Marvel.Difficulty
import Marvel.EncounterCard
import Marvel.EncounterSet
import Marvel.EncounterSet qualified as EncounterSet
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id (IdentityId)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Id (EnemyId (..))
import Marvel.Identity.Types (Field (PlayerIdentityEncounterCards))
import Marvel.Phase
import Marvel.Projection
import Marvel.Query
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
    , scenarioReshufflesThisRound :: Natural
    , scenarioDecks :: HashMap DeckKey Pile
    -- ^ the decks besides the encounter deck, each with its discard pile
    , scenarioCardDecks :: HashMap CardId DeckKey
    -- ^ the deck a card of another deck than the encounter deck is discarded to
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

-- | The deck and its discard pile: the encounter deck's are the scenario's own.
pileL :: DeckKey -> Lens' (Attrs Scenario) Pile
pileL = \case
  EncounterDeckKey ->
    lens (\a -> Pile (scenarioEncounterDeck a) (scenarioDiscard a)) \a (Pile d x) -> a {scenarioEncounterDeck = d, scenarioDiscard = x}
  key ->
    lens (fromMaybe emptyPile . HashMap.lookup key . scenarioDecks) \a p -> a {scenarioDecks = HashMap.insert key p (scenarioDecks a)}

-- | Each deck there is, the encounter deck first.
deckKeys :: Attrs Scenario -> [DeckKey]
deckKeys a = EncounterDeckKey : HashMap.keys (scenarioDecks a)

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
            , scenarioReshufflesThisRound = 0
            , scenarioDecks = mempty
            , scenarioCardDecks = mempty
            }
    }

-- | However it empties, an encounter deck is made again from its discard pile at once,
-- with an acceleration token; with no discard pile to make the encounter deck from, the
-- tokens would be placed without end, and the players lose. The side-scheme deck is made
-- again without penalty; a named deck is not made again.
instance RunMessage (Attrs Scenario) where
  runMessage msg attrs = do
    attrs' <- runQueueT $ runScenarioMessage msg attrs
    -- queued after what the message did, so the game is lost before any of it runs
    runQueueT $ foldlM refill attrs' (deckKeys attrs')
   where
    refill a key = case a ^. pileL key of
      Pile [] [] | key == EncounterDeckKey, notNull (scenarioEncounterDeck attrs) -> a <$ gameOver Lost
      Pile [] discard | notNull discard, not (isNamed key) -> do
        deck' <- shuffleM discard
        let penalty = if isEncounterDeck key then 1 else 0
        when (penalty > 0) addAccelerationToken
        pure $
          a
            & (pileL key .~ Pile deck' [])
            & \a' -> a' {scenarioReshufflesThisRound = scenarioReshufflesThisRound a' + penalty}
      _ -> pure a
    isNamed = \case
      NamedDeckKey _ -> True
      _ -> False

-- | The top card of the encounter deck, which is never empty while its discard pile has
-- cards: with neither, the players have lost.
drawEncounterCard :: (ReverseQueue m) => Attrs Scenario -> m ([EncounterCard], Attrs Scenario)
drawEncounterCard attrs = case scenarioEncounterDeck attrs of
  [] -> ([], attrs) <$ gameOver Lost
  card : deck' -> pure ([card], attrs & encounterDeckL .~ deck')

-- | The card leaves the encounter deck, discard pile or set-aside area, wherever it is.
leaves :: EncounterCard -> Attrs Scenario -> Attrs Scenario
leaves card attrs =
  attrs
    & (setAsideCardsL %~ filter (/= EncounterCard card))
    & (encounterDeckL %~ filter (/= card))
    & (discardL %~ filter (/= card))
    & \a -> a {scenarioDecks = HashMap.map (\(Pile d x) -> Pile (filter (/= card) d) (filter (/= card) x)) (scenarioDecks a)}

-- | The encounter-deck messages, and the player whose area's deck they are on when not
-- "you"'s.
deckMessage :: Message -> Maybe (Maybe IdentityId)
deckMessage = \case
  DealEncounterCard ident -> Just (Just ident)
  DrawAndRevealEncounterCard ident -> Just (Just ident)
  SearchForAndRevealEncounterCard ident _ -> Just (Just ident)
  SearchForAndPutIntoPlayEncounterCard ident _ -> Just (Just ident)
  SearchForAndRevealScheme _ -> Just Nothing
  DealBoost _ -> Just Nothing
  DiscardTopOfEncounterDeck _ _ -> Just Nothing
  ShuffleEncounterDeck -> Just Nothing
  ShuffleIntoEncounterDeck _ -> Just Nothing
  PutOnTopOfEncounterDeck _ -> Just Nothing
  LookAtTopOfEncounterDeck _ _ -> Just Nothing
  DiscardUntil FromEncounterDeck _ _ -> Just Nothing
  _ -> Nothing

-- | A message drawing from the deck, which a deck other than an encounter deck with
-- nothing left in it does not do.
drawsFromDeck :: Message -> Bool
drawsFromDeck = \case
  DealEncounterCard _ -> True
  DrawAndRevealEncounterCard _ -> True
  DealBoost _ -> True
  _ -> False

-- | The encounter-deck message run against the deck given: its cards stand in for the
-- encounter deck's while it runs.
runOnDeck :: (HasCallStack, ReverseQueue m) => DeckKey -> Message -> Attrs Scenario -> m (Attrs Scenario)
runOnDeck EncounterDeckKey msg attrs = runDeckMessage EncounterDeckKey msg attrs
runOnDeck key msg attrs
  | drawsFromDeck msg && not (isEncounterDeck key) && null (pileDeck pile) = pure attrs
  | otherwise = do
      a' <- runDeckMessage key msg (attrs & pileL EncounterDeckKey .~ pile)
      pure $ a' & pileL key .~ (a' ^. pileL EncounterDeckKey) & pileL EncounterDeckKey .~ (attrs ^. pileL EncounterDeckKey)
 where
  pile = attrs ^. pileL key


runScenarioMessage :: (HasCallStack, ReverseQueue m) => Message -> Attrs Scenario -> m (Attrs Scenario)
runScenarioMessage msg attrs = case msg of
  UsingDeck key inner -> runOnDeck key inner attrs
  _ | Just mIdent <- deckMessage msg -> do
    key <- getDeckKeyFor =<< maybe getYou pure mIdent
    runOnDeck key msg attrs
  CreateDeck key cards -> do
    deck' <- shuffleM cards
    let homes = HashMap.fromList [(ecCardId c, key) | key /= EncounterDeckKey, discardsTo key == key, c <- cards]
    pure $ foldr leaves attrs cards & pileL key .~ Pile deck' [] & \a -> a {scenarioCardDecks = scenarioCardDecks a <> homes}
  -- a card set aside leaves the set-aside area for wherever it goes
  RemoveFromGame (CardIdRef cid) -> pure $ attrs & setAsideCardsL %~ filter ((/= cid) . toCardId)
  RemoveDeck EncounterDeckKey -> pure attrs
  -- a defeated villain's cards still in play are his deck's: they have no discard pile
  -- to go to
  RemoveDeck key@(VillainDeckKey _) -> pure attrs {scenarioDecks = HashMap.delete key (scenarioDecks attrs)}
  RemoveDeck key ->
    pure attrs {scenarioDecks = HashMap.delete key (scenarioDecks attrs), scenarioCardDecks = HashMap.filter (/= key) (scenarioCardDecks attrs)}
  -- to the discard pile of its own deck, wherever in the decks it was looked at; a card
  -- of a defeated villain's deck, his deck gone with him, leaves the game
  DiscardedCard (EncounterCard ec) -> do
    let home = fromMaybe EncounterDeckKey (HashMap.lookup (ecCardId ec) (scenarioCardDecks attrs))
    pure $
      if home == EncounterDeckKey || HashMap.member home (scenarioDecks attrs)
        then leaves ec attrs & pileL home %~ \(Pile d x) -> Pile d (ec : x)
        else leaves ec attrs
  -- the Campaign Instructions: a scenario adds its own steps answering this first
  CampaignStep step -> attrs <$ runCampaignStep step
  -- the last villain defeated, the players win; otherwise he leaves play
  ResolveVillainDefeat vid -> do
    areas <- getAreas
    others <- concatMapM (getVillainsInArea . areaId) areas
    if any (/= vid) others then removeFromPlay (VillainRef vid) else gameOver Won
    pure attrs
  _ -> runDeckMessage EncounterDeckKey msg attrs

-- | What the scenario does, the deck messages against the deck given.
runDeckMessage :: (HasCallStack, ReverseQueue m) => DeckKey -> Message -> Attrs Scenario -> m (Attrs Scenario)
runDeckMessage key msg attrs@ScenarioAttrs {..} = case msg of
    StartScenario -> do
      encounterCards <-
        shuffleM
          . (<> scenarioEncounterDeck)
          =<< gatherEncounterSets scenarioEncounterSets
      let
        (mainScheme, remainingMainSchemes) = case scenarioMainSchemes of
          [] -> error "No main schemes"
          x : xs -> (x, xs)
      for_ scenarioVillains addVillain
      addSetupMainScheme mainScheme
      pure $
        attrs
          & encounterDeckL
            .~ encounterCards
          & mainSchemesL
            .~ remainingMainSchemes
    -- the scenario's stages are the first area's: a scheme of an area of its own, as The
    -- Once and Future Kang's stage 3, says itself what its completion does
    NextMainScheme old -> do
      let
        (mainScheme, remainingMainSchemes) = case scenarioMainSchemes of
          [] -> error "No main schemes"
          x : xs -> (x, xs)
      replaceMainScheme old mainScheme
      pure $ attrs & mainSchemesL .~ remainingMainSchemes
    ShuffleEncounterDeck -> do
      deck' <- shuffleM scenarioEncounterDeck
      pure $ attrs & encounterDeckL .~ deck'
    -- from wherever they are: set aside, or the discard pile, as a boost card is
    ShuffleIntoEncounterDeck cards -> do
      deck' <- shuffleM (scenarioEncounterDeck <> cards)
      -- from wherever they were: the set-aside area or the discard pile
      pure $ attrs & encounterDeckL .~ deck' & setAsideCardsL %~ filter (`notElem` map EncounterCard cards) & discardL %~ filter (`notElem` cards) & discardL %~ filter (`notElem` cards)
    -- a card comes out from wherever it was: the set-aside area (a nemesis set's), or
    -- the encounter deck or discard pile searched
    RevealEncounterCard _ card -> pure $ leaves card attrs
    PutEncounterCardIntoPlay _ card -> pure $ leaves card attrs
    AttachEncounterCard card _ -> pure $ leaves card attrs
    BeginPhase PlayerPhase -> do
      players <- getAllPlayers
      for_ players \p -> do
        setActivePlayer p
        identityStep p BeginTurn
      endPhase PlayerPhase
      pure attrs
    EndPhase PlayerPhase -> do
      players <- getAllPlayers
      for_ players \ident -> do
        setActivePlayer ident
        identityStep ident DiscardCards
        identityStep ident DrawOrDiscardToHandLimit
      for_ players (`identityStep` ReadyCards)
      for_ (take 1 players) setActivePlayer
      beginPhase VillainPhase
      pure attrs
    -- in each area its active villain activates against its players, and its hazard
    -- icons deal its players more encounter cards
    BeginPhase VillainPhase -> do
      players <- getAllPlayers
      areas <- getAreas
      hazards <- for areas \area -> do
        here <- filterM (fmap ((== areaId area) . areaId) . getAreaOf) players
        n <- fromIntegral <$> getHazardCountIn (areaId area)
        pure $ if null here then [] else take n (cycle here)
      accelerateMainScheme
      for_ players \p -> do
        setActivePlayer p
        identityStep p VillainAndMinionsActivate
      for_ players dealEncounterCard
      for_ (concat hazards) dealEncounterCard
      revealDealtEncounterCards
      passFirstPlayer
      endPhase VillainPhase
      endRound
      pure attrs
    -- each player in turn reveals the cards dealt them, until none remain
    RevealDealtEncounterCards -> do
      players <- filterM (fmap notNull . project PlayerIdentityEncounterCards) =<< getAllPlayers
      unless (null players) do
        for_ players \p -> do
          setActivePlayer p
          identityStep p RevealEncounterCards
        revealDealtEncounterCards
      pure attrs
    EndRound -> do
      checkWindows [W.Window W.When W.RoundEnded]
      beginRound
      pure attrs {scenarioReshufflesThisRound = 0}
    BeginRound -> do
      beginPhase PlayerPhase
      pure attrs
    DealEncounterCard ident -> do
      (ecs, attrs') <- drawEncounterCard attrs
      for_ ecs (identityStep ident . DealtEncounterCard)
      pure attrs'
    -- ponytail: a deck worn down to surge cards would surge forever, so after a few
    -- reshuffles in a round surge stops; a real table would just stop too
    Surge _ | scenarioReshufflesThisRound >= 3 -> pure attrs
    -- the surge keyword: "When Revealed: Deal yourself 1 facedown encounter card"
    Surge ident -> do
      checkWindows [W.Window W.Would $ W.SurgeResolves ident]
      dealEncounterCard ident
      pure attrs
    DrawAndRevealEncounterCard ident -> do
      (ecs, attrs') <- drawEncounterCard attrs
      for_ ecs (revealEncounterCard ident)
      pure attrs'
    DealBoost target -> do
      (ecs, attrs') <- drawEncounterCard attrs
      case target of
        VillainRef vid -> for_ ecs (villainStep vid . VillainDealtBoost)
        EnemyRef (EnemyVillainId vid) -> for_ ecs (villainStep vid . VillainDealtBoost)
        MinionRef mid -> for_ ecs (minionStep mid . MinionDealtBoost)
        EnemyRef (EnemyMinionId mid) -> for_ ecs (minionStep mid . MinionDealtBoost)
        _ -> error "Can not deal boost to target"
      pure attrs'
    -- a deck emptied partway is fulfilled: the new deck is not discarded from
    DiscardTopOfEncounterDeck n mTarget -> do
      let (discarded, deck') = splitAt (fromIntegral n) scenarioEncounterDeck
      for_ mTarget \target -> withDiscarded target FromEncounterDeck (EncounterCard <$> discarded)
      pure $ attrs & encounterDeckL .~ deck' & discardL %~ (reverse discarded <>)
    -- out of play, from wherever in the decks it was
    SetAside cards -> pure $ foldr leaves attrs [ec | EncounterCard ec <- cards] & setAsideCardsL <>~ cards
    RemovePlayerObligationsFromGame -> pure $ attrs & encounterDeckL %~ filter (not . isPlayerObligation)
    PutOnTopOfEncounterDeck cards -> pure $ attrs & encounterDeckL %~ (cards <>) . filter (`notElem` cards)
    LookAtTopOfEncounterDeck n target -> do
      searchFoundCards target $ EncounterCard <$> take (fromIntegral n) scenarioEncounterDeck
      pure attrs
    DiscardUntil FromEncounterDeck cardMatcher target -> do
      let
        (discarded, remaining) =
          break (cardMatch cardMatcher) scenarioEncounterDeck
      case remaining of
        [] -> pure $ attrs & encounterDeckL .~ [] & discardL %~ (reverse discarded <>)
        x : xs -> do
          withDiscardedMatch target FromEncounterDeck (EncounterCard x)
          pure $ attrs & encounterDeckL .~ xs & discardL %~ (reverse discarded <>)
    SearchForAndRevealScheme cardDef -> do
      ident <- selectJust You
      search ident (CardIs cardDef) (revealEncounterCard ident)
    SearchForAndRevealEncounterCard ident matcher -> search ident matcher (revealEncounterCard ident)
    SearchForAndPutIntoPlayEncounterCard ident matcher -> search ident matcher (putEncounterCardIntoPlay ident)
    _ -> pure attrs
 where
  -- the searching player chooses one of the cards found, then the deck is shuffled
  search ident matcher f = do
    let found = List.nubBy ((==) `on` toCardCode) $ filter (cardMatch matcher) (scenarioEncounterDeck <> scenarioDiscard)
    unless (null found) $ chooseOrRunOne ident [targeting card (f card) | card <- found]
    -- the deck searched
    if key == EncounterDeckKey then shuffleEncounterDeck else fromDeck key shuffleEncounterDeck
    pure attrs
