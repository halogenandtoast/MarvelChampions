module TestImport (
  module X,
  module TestImport,
) where

import Data.HashMap.Strict qualified as HashMap
import Data.IORef as X
import Marvel.Decklist as X
import Marvel.Difficulty as X
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
import Marvel.Villain.Types as X (Villain, villainDamage)
import Marvel.Deck as X
import Marvel.Discard as X
import Marvel.Log as X (LogEntry, entryText)
import System.Random (mkStdGen)
import Test.Hspec as X hiding (shouldBe, shouldReturn, shouldSatisfy)
import Test.Hspec.Expectations.Lifted as X (shouldBe, shouldReturn, shouldSatisfy)
import Control.Monad.IO.Class as X (liftIO)
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Identity (getModifiedHandSize, getModifiedHp)
import Marvel.MainScheme.Types (getMainSchemeThreat)
import Marvel.Minion.Types (getMinionDamage)
import Marvel.Payment.Types (Payment (NoPayment))
import Marvel.Scenario.Types (Field (ScenarioDiscard, ScenarioEncounterDeck))
import Data.Aeson (Result (..), Value (Object), fromJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)

-- | A game dealt from the decks given against the scenario set up, played up to its first
-- question, with the messages it runs kept, the latest first.
dealtGame :: ScenarioSetup -> [DeckList] -> Int -> IO (GameApp, IORef [Text])
dealtGame setup decks seed = do
  (app, ran, _) <- dealtGameLogged setup decks seed
  pure (app, ran)

-- | The same, keeping the log entries it writes too, oldest first.
dealtGameLogged :: ScenarioSetup -> [DeckList] -> Int -> IO (GameApp, IORef [Text], IORef [LogEntry])
dealtGameLogged setup decks seed = do
  scenario <- maybe (error "no such scenario") pure $ lookupScenario setup
  players <- either (error . show) pure . sequence $ evalRand (traverse loadPlayer decks) (mkStdGen seed)
  seated <- maybe (error "no decks") pure $ nonEmpty' [setName ("player " <> tshow n) p | (n, p) <- zip [1 :: Int ..] players]
  ran <- newIORef []
  logged <- newIORef []
  app <- newGameApp (newTable scenario seated) [StartGame] (mkStdGen seed) \case
    Logged e -> modifyIORef' logged (<> [e])
    _ -> pure ()
  let app' = app {appDebugLogger = Just $ DebugLogger \m -> modifyIORef' ran (take 60 . (tshow m :))}
  runGameApp app' runGame
  pure (app', ran, logged)
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

-- * Card tests

-- | A game for testing a card: dealt and played to the start of the first player's
-- turn, mulligans kept.
data TestGame = TestGame {app :: GameApp, ran :: IORef [Text], logged :: IORef [LogEntry], seats :: [IdentityId]}

-- | Against the scenario with the starter decks of the heroes given.
testGameAgainst :: CardCode -> [Text] -> IO TestGame
testGameAgainst villain heroes = do
  let setup = fromMaybe (error "no setup") (recommendedSetup villain)
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

-- | Answer the open question with the first choice that matches, failing the test when
-- there is none.
choose :: (HasCallStack) => Text -> (Choice -> Bool) -> GameAppT ()
choose what p = do
  refreshTurn
  asked >>= \case
    Nothing -> error $ "nothing was asked when trying to " <> show what
    Just (ident, cs) -> case findIndex' p cs of
      Nothing -> error $ "could not " <> show what <> " among " <> show (map describe cs)
      Just n -> answerQuestion ident n
 where
  findIndex' f = fmap fst . headMay . filter (f . snd) . zip [0 ..]
  describe = take 120 . show

-- | Whether the open question offers a matching choice.
offers :: (Choice -> Bool) -> GameAppT Bool
offers p = refreshTurn >> maybe False (any p . snd) <$> asked

-- | Ask the turn question afresh, as a test may have changed the game under it.
refreshTurn :: GameAppT ()
refreshTurn =
  openQuestion >>= \case
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
  TargetLabel t _ -> t == toTarget a
  CardLabel _ c -> isTargetChoice a c
  _ -> False

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

-- | Put a new copy of a card into a player's hand.
putInHand :: IdentityId -> CardCode -> GameAppT PlayerCard
putInHand ident code = do
  cardId <- getRandom
  let card = MkPlayerCard cardId (lookupPlayerCard code) (Just ident) (Just ident)
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
  for_ payment \p -> choose "pay" (isPayWith p)
  whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment

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
  -- a hero with a choice of alter-egos is asked which
  whenM (offers \case ChangeToForm _ -> True; _ -> False) $ choose "pick a form" \case
    ChangeToForm _ -> True
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
  asked >>= \case
    Just (ident, cs) | Just n <- passIndex cs -> answerQuestion ident n >> pass
    _ -> pure ()
 where
  passIndex cs = fmap fst $ headMay $ filter (passing . snd) $ zip [0 ..] cs
  passing = \case
    Label l _ -> l `elem` ["$label.noResponse", "$label.noDefender", "$label.doneDiscarding"]
    _ -> False

-- | Put cards on top of a player's deck, first given on top.
stackDeck :: IdentityId -> [CardCode] -> GameAppT [PlayerCard]
stackDeck ident codes = do
  cards <- for codes \code -> do
    cardId <- getRandom
    pure $ MkPlayerCard cardId (lookupPlayerCard code) (Just ident) (Just ident)
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
  let card = MkPlayerCard cardId (lookupPlayerCard code) (Just ident) (Just ident)
  resolve [PutCardIntoPlay ident card NoPayment Nothing]
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
    pure $ MkPlayerCard cardId (lookupPlayerCard code) (Just ident) (Just ident)
  withGame_ $ entitiesL . playersL . ix ident %~ \(PlayerIdentity a) ->
    PlayerIdentity a {playerIdentityDiscard = Discard (cards <> unDiscard a.playerIdentityDiscard)}
  pure cards

-- | Set a player's damage.
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

encounterDiscardCodes :: GameAppT [CardCode]
encounterDiscardCodes = getsGame (map (toCardCode @EncounterCard) . field ScenarioDiscard . gameScenario)

obligationsInPlay :: GameAppT Int
obligationsInPlay = getsGame (length . gameObligations)

-- | Put new encounter cards on top of the encounter deck, first given on top.
stackEncounterDeck :: [CardDef] -> GameAppT [EncounterCard]
stackEncounterDeck defs = do
  cards <- traverse genEncounterCard defs
  withGame_ \g -> g {gameScenario = restack cards (gameScenario g)}
  pure cards
 where
  -- ponytail: through the scenario's JSON, as the scenario is existential
  restack cards s = case toJSON s of
    Object o
      | Just deck <- KeyMap.lookup "scenarioEncounterDeck" o
      , Success deck' <- fromJSON @[EncounterCard] deck ->
          either error id . parseEither parseJSON . Object $
            KeyMap.insert "scenarioEncounterDeck" (toJSON (cards <> deck')) o
    _ -> error "no encounter deck"

encounterDeckSize :: GameAppT Int
encounterDeckSize = getsGame (length . field ScenarioEncounterDeck . gameScenario)
