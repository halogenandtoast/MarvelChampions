module TestImport (
  module X,
  module TestImport,
) where

import Data.List (nub)
import Marvel.EncounterSet qualified as EncounterSet
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Marvel.MainScheme.Types (getMainSchemeThreat)
import Marvel.Scenario.Types (scenarioDiscard)
import Marvel.SideScheme.Types (getSideSchemeThreat)
import Data.IORef as X
import Marvel.Decklist as X
import Marvel.Difficulty as X
import Marvel.Area as X (Area (..))
import Marvel.Game as X
import Marvel.Game.Run as X
import Marvel.Id as X
import Marvel.Identity as X (setName)
import Marvel.Message as X
import Marvel.Prelude as X
import Marvel.Question as X
import Marvel.Queue as X
import Marvel.Scenario as X
import Marvel.Scenario.Types as X (ScenarioSetup (..))
import Control.Monad.Random (evalRand)
import Marvel.Debug as X
import Marvel.Ability as X (Ability (..))
import Marvel.Card as X hiding (toCard)
import Marvel.Entity as X hiding (isTarget)
import Marvel.Frontend as X
import Marvel.Hand as X
import Marvel.Identity.Types as X
import Marvel.PlayerCard as X (lookupPlayerCard)
import Marvel.Ref as X
import Marvel.Resource.Types as X (Resource (..))
import Marvel.Message.Lifted as X (countLabel)
import Marvel.Villain.Types as X (Villain, villainDamage)
import Marvel.Deck as X
import Marvel.Discard as X
import Marvel.Log as X (LogEntry, entryText)
import System.Random (mkStdGen)
import Control.Exception (SomeException, SomeAsyncException, displayException, evaluate, fromException, throwIO, try, tryJust)
import Test.Hspec as X hiding (shouldBe, shouldMatchList, shouldReturn, shouldSatisfy)
import Test.Hspec.Expectations.Lifted as X (shouldBe, shouldMatchList, shouldReturn, shouldSatisfy)
import Control.Monad.IO.Class as X (liftIO)
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Identity (getModifiedHandSize, getModifiedHp)
import Marvel.Minion.Types (getMinionDamage)
import Marvel.Scenario.Types (Field (ScenarioDiscard, ScenarioEncounterDeck))
import Data.Aeson.Types (parseEither)
import Data.Aeson.Key (Key)

import Marvel.Scenario.Types (Attrs (scenarioEncounterDeck))
import Marvel.Ally.Types as X (Ally, Attrs (..))
import Marvel.Support.Types as X (Support, Attrs (..))
import Marvel.Upgrade.Types as X (Upgrade, Attrs (..))
import Marvel.Minion.Types as X (Minion, Attrs (..))
import Marvel.Damage as X

-- | Fixture invariants should fail with the calling test's location, including
-- when an unexpected empty list would otherwise hide the intended assertion.
testHead :: HasCallStack => [a] -> a
testHead = \case
  value : _ -> value
  [] -> error "test fixture expected a nonempty list"

testTail :: HasCallStack => [a] -> [a]
testTail = \case
  _ : rest -> rest
  [] -> error "test fixture expected a nonempty list before taking its tail"

-- | A game dealt from the decks given against the scenario set up, played up to its first
-- question, with the messages it runs kept, the latest first.
dealtGame :: ScenarioSetup -> [DeckList] -> Int -> IO (GameApp, IORef [Text])
dealtGame setup decks seed = do
  (app, ran, _) <- dealtGameLogged setup decks seed
  pure (app, ran)

-- | The same, keeping the log entries it writes too, oldest first.
dealtGameLogged :: ScenarioSetup -> [DeckList] -> Int -> IO (GameApp, IORef [Text], IORef [LogEntry])
dealtGameLogged = dealtGameWith id

-- | The same, the table changed before it is dealt: given a campaign, say.
dealtGameWith :: (Game -> Game) -> ScenarioSetup -> [DeckList] -> Int -> IO (GameApp, IORef [Text], IORef [LogEntry])
dealtGameWith f setup decks seed = do
  logged <- newIORef []
  (app, ran) <- dealtGameTelling f setup decks seed \case
    Logged e -> modifyIORef' logged (<> [e])
    _ -> pure ()
  pure (app, ran, logged)

-- | The same, what the game tells its clients handed to the function given.
dealtGameTelling :: (Game -> Game) -> ScenarioSetup -> [DeckList] -> Int -> (FrontendMessage -> IO ()) -> IO (GameApp, IORef [Text])
dealtGameTelling f setup decks seed tell = do
  result@(app, _) <- dealtGameBeforeSetup f setup decks seed tell
  runGameApp app pressMainSchemeSetup
  pure result

-- | Stop at the setup button so tests can play setup itself.
dealtGameBeforeSetup :: (Game -> Game) -> ScenarioSetup -> [DeckList] -> Int -> (FrontendMessage -> IO ()) -> IO (GameApp, IORef [Text])
dealtGameBeforeSetup f setup decks seed tell = do
  scenario <- maybe (error "no such scenario") pure $ lookupScenario setup
  players <- either (error . show) pure . sequence $ evalRand (traverse loadPlayer decks) (mkStdGen seed)
  seated <- maybe (error "no decks") pure $ nonEmpty' [setName ("player " <> tshow n) p | (n, p) <- zip [1 :: Int ..] players]
  ran <- newIORef []
  app <- newGameApp (f $ newTable scenario seated) [StartGame] (mkStdGen seed) tell
  -- a message whose show throws is kept as that error, so a crash's trail can be shown
  let keep :: (Show a) => a -> IO ()
      keep m = do
        shown <- tryJust
          (\e -> case fromException e :: Maybe SomeAsyncException of
            Just _ -> Nothing
            Nothing -> Just (e :: SomeException))
          (evaluate (force' (tshow m)))
        modifyIORef' ran (take 200 . (either (\(e :: SomeException) -> "<unshowable: " <> T.pack (displayException e) <> ">") id shown :))
      force' t = T.length t `seq` t
  let app' = app {appDebugLogger = Just $ DebugLogger keep}
  -- a crash while dealing says what ran up to it, latest last
  dealt <- try (runGameApp app' runGame)
  for_ (either Just (const Nothing) dealt) \(err :: SomeException) -> do
    latest <- readIORef ran
    throwIO $ userError $ unlines $ ("dealing: " <> displayException err) : map T.unpack (reverse latest)
  pure (app', ran)
 where
  nonEmpty' = \case
    [] -> Nothing
    (x : xs) -> Just (x :| xs)

starter :: Text -> DeckList
starter hero = fromMaybe (error $ "no starter deck for " <> show hero) $ lookupStarterDeck hero

-- | Who is asked what, if anyone is.
openQuestion :: GameAppT (Maybe (IdentityId, Question))
openQuestion = getsGame (headMay' . HashMap.toList . gameQuestion)
 where
  headMay' = \case
    [] -> Nothing
    (x : _) -> Just x

questionSize :: Question -> Int
questionSize = \case
  ChooseOne cs -> length cs
  ChooseN _ cs -> length cs
  ChooseOneAtATime cs -> length cs
  ChoosePlayerOrder (Unsorted xs) _ -> length xs
  QuestionLabel _ _ q -> questionSize q
  Read _ q -> questionSize q
  Searched _ q -> questionSize q

-- * Card tests

-- | A game for testing a card: dealt and played to the start of the first player's
-- turn, mulligans kept.
data TestGame = TestGame {app :: GameApp, ran :: IORef [Text], logged :: IORef [LogEntry], seats :: [IdentityId]}

-- | A concrete choice for tests of scenarios whose product names no default.
-- This fixture is deliberately separate from recommendations exposed to players.
withTestModulars :: ScenarioSetup -> ScenarioSetup
withTestModulars setup
  | setup.scenarioCode == "24001" = setup {modulars = take 7 (nub (filter (`elem` EncounterSet.modularSets) setup.modulars <> chosen)) <> filter (`notElem` EncounterSet.modularSets) setup.modulars}
  | otherwise = setup
 where
  chosen = [EncounterSet.BombScare, EncounterSet.MastersOfEvil, EncounterSet.UnderAttack, EncounterSet.GoblinGimmicks, EncounterSet.Temporal, EncounterSet.Anachronauts, EncounterSet.MasterOfTime]

-- | Against the scenario with the starter decks of the heroes given.
testGameAgainst :: CardCode -> [Text] -> IO TestGame
testGameAgainst villain heroes = do
  let setup = withTestModulars $ fromMaybe (error "no setup") (recommendedSetup villain)
  (app, ran, logged) <- dealtGameLogged setup (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

-- | Spider-Man and friends against Rhino.
testGame :: [Text] -> IO TestGame
testGame = testGameAgainst "01094"

inGame :: TestGame -> GameAppT a -> IO a
inGame tg = runGameApp tg.app

-- | Play through setup, answering with first choices (the first seat goes first, every
-- hand is kept), to the first player's first turn.
toFirstTurn :: GameAppT ()
toFirstTurn = go (200 :: Int)
 where
  go 0 = error "setup did not reach a turn"
  go n =
    openQuestion >>= \case
      Just (_, q) | EndTurn `elem` choicesOf q -> pure ()
      Just (ident, _) -> answerQuestion ident 0 >> go (n - 1)
      Nothing -> error "no question during setup"

-- | Who is asked, and the choices, without the prompt.
asked :: GameAppT (Maybe (IdentityId, [Choice]))
asked = fmap (second choicesOf) <$> openQuestion

choicesOf :: Question -> [Choice]
choicesOf = \case
  ChooseOne cs -> cs
  ChooseN _ cs -> cs
  ChooseOneAtATime cs -> cs
  ChoosePlayerOrder {} -> []
  QuestionLabel _ _ q -> choicesOf q
  Read _ q -> choicesOf q
  Searched _ q -> choicesOf q

-- | Answer the open question with the first choice that matches, failing the test when
-- there is none.
choose :: (HasCallStack) => Text -> (Choice -> Bool) -> GameAppT ()
choose what p = do
  refreshTurn
  -- a player who was the only choice was chosen for them, as a player would have
  autoChosen <- any (p . flip TargetLabel [] . IdentityRef) <$> players
  asked >>= \case
    Nothing | autoChosen -> pure ()
    Nothing -> error $ "nothing was asked when trying to " <> show what
    Just (ident, cs) -> case findIndex' p cs of
      Nothing | notNull cs && all (isLabelStarting "$label.spendResourceAllocation") cs -> do
        -- Unspecified surplus allocation is plumbing for most card tests. An
        -- explicit allocation predicate matches above and is never overridden.
        answerQuestion ident 0
        choose what p
      Nothing | autoChosen -> pure ()
      Nothing -> error $ "could not " <> show what <> " among " <> show (map describe cs)
      Just n -> answerQuestion ident n >> pressSpecials
 where
  findIndex' f = fmap fst . headMay . filter (f . snd) . zip [0 ..]
  describe = take 120 . show

-- | Whether the open question offers a matching choice.
offers :: (Choice -> Bool) -> GameAppT Bool
offers p = refreshTurn >> maybe False (any p . snd) <$> asked

-- | Ask the turn question afresh, as a test may have changed the game under it. A When
-- Revealed the game waits on is resolved first, as the player would click it.
refreshTurn :: GameAppT ()
refreshTurn =
  pressWhenRevealed >> openQuestion >>= \case
    Just (ident, QuestionLabel "$label.yourTurn" _ _) -> do
      withGame_ $ questionL .~ mempty
      push $ IdentityMessage ident PlayerTurnOption
      runGame
    _ -> pure ()

isPlay :: PlayerCard -> Choice -> Bool
isPlay c = \case
  PlayCard c' _ -> c == c'
  TargetLabel _ cs -> any (isPlay c) cs
  _ -> False

isLabel :: Text -> Choice -> Bool
isLabel l = \case
  Label l' _ -> l == l'
  _ -> False

isTargetChoice :: (IsRef a) => a -> Choice -> Bool
isTargetChoice a = \case
  TargetLabel t _ -> concrete t == concrete (toTarget a)
  CardLabel _ c -> isTargetChoice a c
  _ -> False
 where
  -- an enemy, scheme or character is the same target by any of its ids
  concrete = \case
    EnemyRef e -> enemyRef e
    SchemeRef s -> schemeRef s
    CharacterRef c -> characterRef c
    t -> t

-- | Paying with a card from hand.
isPayWith :: PlayerCard -> Choice -> Bool
isPayWith c = \case
  PayWithCard c' -> c == c'
  _ -> False

isAbilityOf :: (IsRef a) => a -> Choice -> Bool
isAbilityOf a = \case
  UseAbility ab -> abilitySource ab == toSource a
  _ -> False

isFinishPayment :: Choice -> Bool
isFinishPayment = \case
  FinishPayment -> True
  _ -> False

-- | Declare the first wild resource paid that is not yet that type as it, while paying.
declareWildAs :: (HasCallStack) => Resource -> GameAppT ()
declareWildAs r = choose "declare a wild resource" \case
  DeclareWild _ r' -> r == r'
  _ -> False

-- | Play a card from hand, paying with the cards given and declaring a wild resource
-- among them as the type given.
playDeclaring :: (HasCallStack) => PlayerCard -> [PlayerCard] -> Resource -> GameAppT ()
playDeclaring card payment r = do
  choose "play the card" (isPlay card)
  for_ payment \p -> choose "pay" (isPayWith p)
  declareWildAs r
  choose "finish paying" isFinishPayment

-- | Put a new copy of a card into a player's hand.
putInHand :: IdentityId -> CardCode -> GameAppT PlayerCard
putInHand ident code = do
  cardId <- getRandom
  def <- lookupPlayerCardInGame code
  let card = MkPlayerCard cardId def (Just ident) (Just ident)
  withGame_ $ entitiesL . playersL . ix ident %~ \(PlayerIdentity a) ->
    PlayerIdentity a {playerIdentityHand = Hand (card : unHand a.playerIdentityHand)}
  pure card

-- | Empty a player's hand, for a test to fill.
clearHand :: IdentityId -> GameAppT ()
clearHand ident = withGame_ $ entitiesL . playersL . ix ident %~ \(PlayerIdentity a) ->
  PlayerIdentity a {playerIdentityHand = Hand []}

-- | Play a card from hand, paying with the cards given, in order.
playWith :: (HasCallStack) => PlayerCard -> [PlayerCard] -> GameAppT ()
playWith card payment = do
  choose "play the card" (isPlay card)
  payWith payment

-- | Play the card on the target chosen as it is played, then pay for it.
playOn :: (HasCallStack, IsRef a) => PlayerCard -> a -> [PlayerCard] -> GameAppT ()
playOn card target payment = do
  choose "play the card" (isPlay card)
  choose "play it on the target" (isTargetChoice target)
  payWith payment

payWith :: (HasCallStack) => [PlayerCard] -> GameAppT ()
payWith payment = do
  for_ payment \p -> choose "pay" (isPayWith p)
  whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
  -- Most card tests do not distinguish which surplus icons are wasted. Tests
  -- that do pay cards manually and select their desired satisfying allocation.
  whenM (offers $ isLabelStarting "$label.spendResourceAllocation") $
    choose "allocate the resources actually spent" (isLabelStarting "$label.spendResourceAllocation")

getPlayer :: IdentityId -> GameAppT PlayerIdentity
getPlayer = getIdentity

getVillain :: GameAppT Villain
getVillain = getsGame (fromMaybe (error "no villain") . headMay . HashMap.elems . gameVillains)

-- | Change the player's form with the identity's action.
changeForm :: (HasCallStack) => IdentityId -> GameAppT ()
changeForm ident = do
  choose "change form" \case
    UseAbility ab -> ChangeForm `elem` abilityChoices ab && abilitySource ab == toSource ident
    _ -> False
  -- a hero with a choice of forms is asked which
  whenM (offers $ isFormChoice Nothing) $ choose "pick a form" (isFormChoice Nothing)

-- | Change the player's form with the identity's action, to this side: Ant-Man has three.
changeFormTo :: (HasCallStack) => IdentityId -> Side -> GameAppT ()
changeFormTo ident side = do
  choose "change form" \case
    UseAbility ab -> ChangeForm `elem` abilityChoices ab && abilitySource ab == toSource ident
    _ -> False
  choose "pick a form" (isFormChoice $ Just side)

-- | A choice of the form to change to, this one or any; Ant-Man's show their face.
isFormChoice :: Maybe Side -> Choice -> Bool
isFormChoice side = \case
  CardLabel _ c -> isFormChoice side c
  ChangeToForm s -> maybe True (== s) side
  _ -> False

-- | End the turn, and keep every card at the end of the player phase.
endTurn :: (HasCallStack) => GameAppT ()
endTurn = do
  choose "end the turn" \case
    EndTurn -> True
    _ -> False
  keepCards

-- | Decline every discard and every response until something else is asked.
keepCards :: GameAppT ()
keepCards =
  openQuestion >>= \case
    Just (ident, QuestionLabel "$label.discardAnyCards" _ _) -> answerQuestion ident 0 >> keepCards
    Just (ident, QuestionLabel "$label.discardToHandSize" _ _) -> answerQuestion ident 0 >> keepCards
    _ -> pure ()

-- | Decline responses and defending until something else is asked.
pass :: GameAppT ()
pass =
  pressWhenRevealed >> asked >>= \case
    Just (ident, cs) | Just n <- passIndex cs -> answerQuestion ident n >> pass
    _ -> pure ()
 where
  passIndex cs = fmap fst $ headMay $ filter (passing . snd) $ zip [0 ..] cs
  passing = \case
    Label l _ -> l `elem` ["$label.noResponse", "$label.noDefender", "$label.declineToDefend", "$label.doneDiscarding"]
    _ -> False

-- | Put cards on top of a player's deck, first given on top.
stackDeck :: IdentityId -> [CardCode] -> GameAppT [PlayerCard]
stackDeck ident codes = do
  cards <- for codes \code -> do
    cardId <- getRandom
    def <- lookupPlayerCardInGame code
    pure $ MkPlayerCard cardId def (Just ident) (Just ident)
  withGame_ $ entitiesL . playersL . ix ident %~ \(PlayerIdentity a) ->
    PlayerIdentity a {playerIdentityDeck = Deck (cards <> unDeck a.playerIdentityDeck)}
  pure cards

handOf :: IdentityId -> GameAppT [PlayerCard]
handOf ident = unHand . (.playerIdentityHand) . toAttrs <$> getPlayer ident

discardOf :: IdentityId -> GameAppT [PlayerCard]
discardOf ident = unDiscard . (.playerIdentityDiscard) . toAttrs <$> getPlayer ident

damageOf :: IdentityId -> GameAppT Natural
damageOf ident = (.playerIdentityDamage) . toAttrs <$> getPlayer ident

villainDamageNow :: GameAppT Natural
villainDamageNow = villainDamage <$> getVillain

-- | Run these messages now, answering whatever they ask with its first choice (the first
-- forced ability, or not responding), until the game is back at the question it was
-- waiting on or is over.
resolve :: [Message] -> GameAppT ()
resolve msgs = do
  waiting <- openQuestion
  withGame_ $ questionL .~ mempty
  pushAll $ msgs <> [Ask ident q | Just (ident, q) <- [waiting]]
  runGameMessages
  settle waiting (200 :: Int)
 where
  settle waiting n = do
    current <- openQuestion
    finished <- getsGame (isFinished . gameState)
    case current of
      Just (ident, _) | n > 0, current /= waiting, not finished -> do
        answerQuestion ident 0
        settle waiting (n - 1)
      _ -> pure ()
  isFinished = \case
    Finished _ -> True
    _ -> False

-- | A fresh copy of an encounter card.
encounterCard :: CardDef -> GameAppT EncounterCard
encounterCard = genEncounterCard

-- | The players in player order.
players :: GameAppT [IdentityId]
players = getsGame gamePlayerOrder


-- | The first seat's player.
player1 :: TestGame -> IdentityId
player1 tg = fromMaybe (error "no players") (headMay tg.seats)

-- | Put a new copy of a player card into play for a player, as if played for free.
putIntoPlay :: IdentityId -> CardCode -> GameAppT PlayerCard
putIntoPlay ident code = do
  cardId <- getRandom
  def <- lookupPlayerCardInGame code
  let card = MkPlayerCard cardId def (Just ident) (Just ident)
  resolve [PutCardIntoPlay ident card NoPayment Nothing True]
  pure card

upgradeIdOf :: PlayerCard -> UpgradeId
upgradeIdOf = UpgradeId . unCardId . pcCardId

allyIdOf :: PlayerCard -> AllyId
allyIdOf = AllyId . unCardId . pcCardId

supportIdOf :: PlayerCard -> SupportId
supportIdOf = SupportId . unCardId . pcCardId

-- | Put new copies of cards on top of a player's discard pile, first given on top.
stackDiscard :: IdentityId -> [CardCode] -> GameAppT [PlayerCard]
stackDiscard ident codes = do
  cards <- for codes \code -> do
    cardId <- getRandom
    def <- lookupPlayerCardInGame code
    pure $ MkPlayerCard cardId def (Just ident) (Just ident)
  withGame_ $ entitiesL . playersL . ix ident %~ \(PlayerIdentity a) ->
    PlayerIdentity a {playerIdentityDiscard = Discard (cards <> unDiscard a.playerIdentityDiscard)}
  pure cards

-- | Set a player's damage.
-- * More card test helpers

-- | Run these messages now, ahead of the question that was waiting, without answering
-- anything: the test answers what they ask, and the waiting question comes back after.
runNow :: [Message] -> GameAppT ()
runNow msgs = do
  waiting <- openQuestion
  withGame_ $ questionL .~ mempty
  pushAll $ msgs <> [Ask ident q | Just (ident, q) <- [waiting]]
  runGame
  pressWhenRevealed

-- | Deal through the explicit setup button before a test plays its starting hand.
pressMainSchemeSetup :: GameAppT ()
pressMainSchemeSetup = openQuestion >>= \case
  Just (ident, QuestionLabel "$label.setupMainScheme" _ _) -> answerQuestion ident 0 >> pressMainSchemeSetup
  _ -> pure ()

-- | Resolve acknowledgement prompts as a player clicking the card would. Tests
-- about the prompt itself inspect openQuestion before using these helpers.
pressWhenRevealed :: GameAppT ()
pressWhenRevealed = openQuestion >>= \case
  Just (ident, QuestionLabel label _ _)
    | label `elem` ["$label.resolveWhenRevealed", "$label.resolveWhenDefeated", "$label.resolveSpecial"] ->
        answerQuestion ident 0 >> pressWhenRevealed
  _ -> pure ()

-- | Card behavior tests acknowledge a Special reached after their chosen action.
-- Prompt tests use answerQuestion directly to inspect each pause.
pressSpecials :: GameAppT ()
pressSpecials = openQuestion >>= \case
  Just (ident, QuestionLabel "$label.resolveSpecial" _ _) -> answerQuestion ident 0 >> pressSpecials
  _ -> pure ()

-- | Set the damage on a player's identity.
setDamage :: IdentityId -> Natural -> GameAppT ()
setDamage ident n = withGame_ $ entitiesL . playersL . ix ident %~ \(PlayerIdentity a) ->
  PlayerIdentity a {playerIdentityDamage = n}

-- | An ability with this label, such as an identity's named ability.
isLabeledAbility :: Text -> Choice -> Bool
isLabeledAbility l = \case
  UseAbility ab -> abilityLabel ab == Just l
  _ -> False

handSizeOf :: IdentityId -> GameAppT Natural
handSizeOf ident = getModifiedHandSize =<< getPlayer ident

hitPointsOf :: IdentityId -> GameAppT Natural
hitPointsOf ident = getModifiedHp =<< getPlayer ident

isExhaustedPlayer :: IdentityId -> GameAppT Bool
isExhaustedPlayer ident = (.playerIdentityExhausted) . toAttrs <$> getPlayer ident

upgradesOf :: IdentityId -> GameAppT [UpgradeId]
upgradesOf ident = toList . (.playerIdentityUpgrades) . toAttrs <$> getPlayer ident

upgradeIsExhausted :: UpgradeId -> GameAppT Bool
upgradeIsExhausted uid = getsGame (maybe False isExhausted . HashMap.lookup uid . gameUpgrades)

mainSchemeThreatNow :: GameAppT Natural
mainSchemeThreatNow = getsGame (sum . map getMainSchemeThreat . HashMap.elems . gameMainSchemes)

minionDamages :: GameAppT [Natural]
minionDamages = getsGame (map getMinionDamage . HashMap.elems . gameMinions)

-- | The main scheme's id.
mainSchemeId :: GameAppT MainSchemeId
mainSchemeId = getsGame (fromMaybe (error "no main scheme") . headMay . HashMap.keys . gameMainSchemes)

-- | Run these messages, stopping at the first thing they ask for the test to answer; the
-- question the game was waiting on is asked again after them.
start :: [Message] -> GameAppT ()
start msgs = do
  waiting <- openQuestion
  withGame_ $ questionL .~ mempty
  pushAll $ msgs <> [Ask ident q | Just (ident, q) <- [waiting]]
  runGameMessages
  pressWhenRevealed

encounterDiscardCodes :: GameAppT [CardCode]
encounterDiscardCodes = getsGame (map (toCardCode @EncounterCard) . field ScenarioDiscard . gameScenario)

obligationsInPlay :: GameAppT Int
obligationsInPlay = getsGame (length . gameObligations)

-- | Put new encounter cards on top of the encounter deck, first given on top.
stackEncounterDeck :: [CardDef] -> GameAppT [EncounterCard]
stackEncounterDeck defs = do
  cards <- traverse genEncounterCard defs
  overEncounterPile "scenarioEncounterDeck" (cards <>)
  pure cards

-- | Make the encounter deck and discard pile exactly new copies of the cards given, the
-- first on top.
setEncounterDeck :: [CardDef] -> [CardDef] -> GameAppT ([EncounterCard], [EncounterCard])
setEncounterDeck deckDefs discardDefs = do
  deck <- traverse genEncounterCard deckDefs
  discard <- traverse genEncounterCard discardDefs
  overEncounterPile "scenarioEncounterDeck" (const deck)
  overEncounterPile "scenarioDiscard" (const discard)
  pure (deck, discard)

-- ponytail: through the scenario's JSON, as the scenario is existential
overEncounterPile :: Key -> ([EncounterCard] -> [EncounterCard]) -> GameAppT ()
overEncounterPile key f = withGame_ \g -> g {gameScenario = restack (gameScenario g)}
 where
  restack s = case toJSON s of
    Object o
      | Just pile <- KeyMap.lookup key o
      , Success pile' <- fromJSON @[EncounterCard] pile ->
          either error id . parseEither parseJSON . Object $ KeyMap.insert key (toJSON (f pile')) o
    _ -> error "no encounter pile"

accelerationTokensNow :: GameAppT Natural
accelerationTokensNow = getsGame (sum . map areaAccelerationTokens . gameAreas)

encounterDeckSize :: GameAppT Int
encounterDeckSize = getsGame (length . field ScenarioEncounterDeck . gameScenario)

playerAttrs :: IdentityId -> GameAppT (Attrs PlayerIdentity)
playerAttrs ident = toAttrs <$> getPlayer ident

mainSchemeThreat :: GameAppT Natural
mainSchemeThreat = getsGame (sum . map getMainSchemeThreat . HashMap.elems . gameMainSchemes)

villainIdNow :: GameAppT VillainId
villainIdNow = toId <$> getVillain

-- | Whether the choice is the "no response" choice of a response window.
isNoResponse :: Choice -> Bool
isNoResponse = isLabel "$label.noResponse"

-- | Whether the choice is declaring no defender.
isNoDefender :: Choice -> Bool
isNoDefender = isLabel "$label.noDefender"

-- | The choices asked now, for a test to see when it fails.
showAsked :: GameAppT ()
showAsked = asked >>= liftIO . print . fmap (second (map (take 160 . show)))

-- | Whether the choice uses an ability of the ally, support or upgrade played from this
-- card.
isAbilityOfCard :: PlayerCard -> Choice -> Bool
isAbilityOfCard c = \case
  UseAbility ab -> abilitySource ab `elem` [AllyRef (AllyId u), SupportRef (SupportId u), UpgradeRef (UpgradeId u)]
  _ -> False
 where
  u = unCardId (pcCardId c)

-- | Whether the choice uses any forced or optional ability.
isAnyAbility :: Choice -> Bool
isAnyAbility = \case
  UseAbility _ -> True
  _ -> False

encounterDiscard :: GameAppT [EncounterCard]
encounterDiscard = getsGame (scenarioDiscard . toAttrs . gameScenario)

-- | Reveal an encounter card for the player now, answering nothing.
revealNow :: IdentityId -> CardDef -> GameAppT EncounterCard
revealNow ident def = do
  card <- encounterCard def
  runNow [RevealEncounterCard ident card]
  pure card

upgradeOf :: PlayerCard -> UpgradeId
upgradeOf = UpgradeId . unCardId . pcCardId

minionOf :: EncounterCard -> MinionId
minionOf = MinionId . unCardId . ecCardId

sideSchemeOf :: EncounterCard -> SideSchemeId
sideSchemeOf = SideSchemeId . unCardId . ecCardId

-- | Whether the player's identity is stunned, confused, exhausted.
isStunned, isConfused, isExhaustedNow :: IdentityId -> GameAppT Bool
isStunned ident = (.playerIdentityStunned) <$> playerAttrs ident
isConfused ident = (.playerIdentityConfused) <$> playerAttrs ident
isExhaustedNow ident = (.playerIdentityExhausted) <$> playerAttrs ident

upgradeCodes :: GameAppT [CardCode]
upgradeCodes = getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameUpgrades)

-- | Whether the choice is a label starting with this text.
isLabelStarting :: Text -> Choice -> Bool
isLabelStarting l = \case
  Label l' _ -> l `T.isPrefixOf` l'
  _ -> False

-- | Whether the choice uses the ability with this label.
isNamedAbility :: Text -> Choice -> Bool
isNamedAbility l = \case
  UseAbility ab -> abilityLabel ab == Just l
  _ -> False

-- | Whether the choice is the player defending against this enemy.
isDefend :: Choice -> Bool
isDefend = \case
  Defend _ -> True
  _ -> False

isEndTurn :: Choice -> Bool
isEndTurn = \case
  EndTurn -> True
  _ -> False

mainSchemeIdNow :: GameAppT MainSchemeId
mainSchemeIdNow = getsGame (HashMap.keys . gameMainSchemes) >>= \case
  [m] -> pure m
  _ -> error "expected one main scheme"

sideSchemeThreat :: SideSchemeId -> GameAppT Natural
sideSchemeThreat sid = getsGame (maybe 0 getSideSchemeThreat . HashMap.lookup sid . gameSideSchemes)

supportOf :: PlayerCard -> SupportId
supportOf = SupportId . unCardId . pcCardId

-- | The attributes of every ally in play with this card code.
alliesWith :: CardCode -> GameAppT [Attrs Ally]
alliesWith code = getsGame (filter ((== code) . toCardCode . allyCardDef) . map toAttrs . HashMap.elems . gameAllies)

-- | The one ally in play with this card code.
allyIn :: (HasCallStack) => CardCode -> GameAppT (Attrs Ally)
allyIn code = alliesWith code >>= \case
  [a] -> pure a
  as -> error $ "expected one ally " <> show code <> ", found " <> show (length as)

isAllyAttack :: Attrs Ally -> Choice -> Bool
isAllyAttack a = \case
  UseAbility ab -> AllyAttack (allyId a) `elem` abilityChoices ab
  _ -> False

isAllyThwart :: Attrs Ally -> Choice -> Bool
isAllyThwart a = \case
  UseAbility ab -> AllyThwart (allyId a) `elem` abilityChoices ab
  _ -> False

-- | The hero's basic attack or thwart.
isHeroAttack, isHeroThwart :: IdentityId -> Choice -> Bool
isHeroAttack ident = \case
  UseAbility ab -> Attack `elem` abilityChoices ab && abilitySource ab == toSource ident
  _ -> False
isHeroThwart ident = \case
  UseAbility ab -> Thwart `elem` abilityChoices ab && abilitySource ab == toSource ident
  _ -> False

supportsWith :: CardCode -> GameAppT [Attrs Support]
supportsWith code = getsGame (filter ((== code) . toCardCode . supportCardDef) . map toAttrs . HashMap.elems . gameSupports)

upgradesWith :: CardCode -> GameAppT [Attrs Upgrade]
upgradesWith code = getsGame (filter ((== code) . toCardCode . upgradeCardDef) . map toAttrs . HashMap.elems . gameUpgrades)

-- | Reveal a minion from the encounter deck to the player, returning it.
addMinion :: IdentityId -> CardDef -> GameAppT MinionId
addMinion ident def = do
  before <- getsGame (HashMap.keys . gameMinions)
  card <- encounterCard def
  resolve [RevealEncounterCard ident card]
  getsGame (filter (`notElem` before) . HashMap.keys . gameMinions) >>= \case
    [m] -> pure m
    _ -> error "the minion did not enter play"

-- | A minion's damage, or Nothing once it has left play.
minionDamageOf :: MinionId -> GameAppT (Maybe Natural)
minionDamageOf m = getsGame (fmap (minionDamage . toAttrs) . HashMap.lookup m . gameMinions)

placeMainThreat :: Natural -> GameAppT ()
placeMainThreat n = do
  m <- mainSchemeId
  resolve [MainSchemeMessage m (MainSchemePlaceThreat n)]

-- | Whether the player is exhausted.
exhaustedNow :: IdentityId -> GameAppT Bool
exhaustedNow ident = (.playerIdentityExhausted) . toAttrs <$> getPlayer ident

-- | Answer with the first choice that matches when it is offered.
chooseIf :: (Choice -> Bool) -> GameAppT ()
chooseIf p = do
  present <- offers p
  if present
    then choose "optional choice" p
    else asked >>= \case
      Just (ident, cs) | notNull cs && all (isLabelStarting "$label.spendResourceAllocation") cs ->
        answerQuestion ident 0 >> chooseIf p
      _ -> pure ()

-- | Put a new copy of a card into a player's discard pile.
putInDiscard :: IdentityId -> CardCode -> GameAppT PlayerCard
putInDiscard ident code = do
  cardId <- getRandom
  def <- lookupPlayerCardInGame code
  let card = MkPlayerCard cardId def (Just ident) (Just ident)
  withGame_ $ entitiesL . playersL . ix ident %~ \(PlayerIdentity a) ->
    PlayerIdentity a {playerIdentityDiscard = Discard (card : unDiscard a.playerIdentityDiscard)}
  pure card

-- | The second seat's player.
player2 :: TestGame -> IdentityId
player2 tg = case tg.seats of
  (_ : p : _) -> p
  _ -> error "no second player"

-- | The encounter discard pile, the latest first.
encounterDiscardNow :: GameAppT [EncounterCard]
encounterDiscardNow = getsGame (scenarioDiscard . toAttrs . gameScenario)

scenarioEncounterDeckNow :: GameAppT [EncounterCard]
scenarioEncounterDeckNow = getsGame (scenarioEncounterDeck . toAttrs . gameScenario)

-- | The facedown encounter cards dealt to a player, in the order dealt.
dealtTo :: IdentityId -> GameAppT [EncounterCard]
dealtTo p = (.playerIdentityEncounterCards) <$> playerAttrs p

-- | Pick the main scheme, as a player picks the scheme their card thwarts.
chooseMainScheme :: (HasCallStack) => GameAppT ()
chooseMainScheme = mainSchemeIdNow >>= \m -> choose "the main scheme" (isTargetChoice m)

-- | Pick the villain, as a player picks the enemy their card damages.
chooseVillain :: (HasCallStack) => GameAppT ()
chooseVillain = villainIdNow >>= \v -> choose "the villain" (isTargetChoice v)
