module Marvel.Game where

import Marvel.Prelude

import Control.Monad.Catch
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Exception
import Marvel.Identity
import Marvel.Message
import Marvel.Player
import Marvel.Queue
import Marvel.Scenario
import Marvel.Villain

data Phase = PlayerPhase | VillainPhase
  deriving stock Show

data GameState = Unstarted | InProgress | Finished
  deriving stock Show

data Game = Game
  { gamePhase :: Phase
  , gameState :: GameState
  , -- players in player order
    gamePlayers :: [Player]
  , gameQuestion :: HashMap IdentityId Question
  , gameVillains :: HashMap VillainId Villain
  , gameScenario :: Scenario
  }
  deriving stock Show

choosePlayerOrder
  :: MonadGame env m => IdentityId -> Unsorted Player -> Sorted Player -> m ()
choosePlayerOrder ident (Unsorted xs) (Sorted ys) = chooseOrRunOne
  ident
  (toChoice <$> xs)
 where
  toChoice x = cardLabel x [ChoosePlayerOrder ident (unsorted x) (sorted x)]
  unsorted x = Unsorted $ filter (/= x) xs
  sorted x = Sorted $ ys <> [x]

runGameMessage :: MonadGame env m => Message -> Game -> m Game
runGameMessage msg g = case msg of
  StartGame -> do
    push StartScenario
    case gamePlayers g of
      [] -> throwM NoPlayers
      players@(p : _) -> choosePlayerOrder (toId p) (Unsorted players) mempty
    pure $ g { gameState = InProgress }
  ChoosePlayerOrder _ (Unsorted []) (Sorted ys) ->
    pure $ g { gamePlayers = ys }
  ChoosePlayerOrder _ (Unsorted [x]) (Sorted ys) -> do
    pure $ g { gamePlayers = ys ++ [x] }
  ChoosePlayerOrder ident unsorted sorted ->
    g <$ choosePlayerOrder ident unsorted sorted
  AddVillain cardCode -> do
    villainId <- getRandom
    case lookupVillain cardCode villainId of
      Just villain -> pure $ g & villainsL . at villainId ?~ villain
      Nothing -> throwM $ MissingCardCode "AddVillain" cardCode
  _ -> pure g

instance RunMessage Game where
  runMessage msg g = traverseOf scenarioL (runMessage msg) g
    >>= runGameMessage msg

chooseOne :: MonadGame env m => IdentityId -> [Choice] -> m ()
chooseOne ident msgs = push (Ask ident $ ChooseOne msgs)

chooseOrRunOne :: MonadGame env m => IdentityId -> [Choice] -> m ()
chooseOrRunOne ident = \case
  [] -> throwM NoChoices
  [choice] -> pushAll $ choiceMessages choice
  choices -> push (Ask ident $ ChooseOne choices)

cardLabel :: HasCardCode a => a -> [Message] -> Choice
cardLabel a = CardLabel (toCardCode a)

playersL :: Lens' Game [Player]
playersL = lens gamePlayers \m x -> m { gamePlayers = x }

questionL :: Lens' Game (HashMap IdentityId Question)
questionL = lens gameQuestion \m x -> m { gameQuestion = x }

scenarioL :: Lens' Game Scenario
scenarioL = lens gameScenario \m x -> m { gameScenario = x }

villainsL :: Lens' Game (HashMap VillainId Villain)
villainsL = lens gameVillains \m x -> m { gameVillains = x }

class HasGame a where
  gameL :: Lens' a (IORef Game)

-- | Create a new game
-- Breaking down setup:
-- 1. Select Identities: This should be done and provided via the frontend
-- 2. Set Hit Points: Use startingHP from Identity
-- 3. Select First Player: Handle via Game Messages
-- 4. Set Aside Obligations TODO later
-- 5. Set Aside Nemesis Sets TODO later
-- 6. Shuffle Player Decks TODO later
-- 7. Collect Tokens and Status Cards NOOP
-- 8. Select Villain
-- 9. Set villain's head count
newGame :: Scenario -> Game
newGame = Game PlayerPhase Unstarted mempty mempty mempty

addPlayer :: MonadGame env m => Player -> m ()
addPlayer player = withGame_ $ playersL %~ (player :)

withGame :: MonadGame env m => (Game -> (Game, a)) -> m a
withGame body = do
  game <- asks $ view gameL
  atomicModifyIORef' game body

withGame_ :: MonadGame env m => (Game -> Game) -> m ()
withGame_ body = withGame ((, ()) . body)

class
  ( MonadCatch m
  , MonadThrow m
  , MonadIO m
  , MonadReader env m
  , HasGame env
  , HasQueue env
  , MonadRandom m
  )
  => MonadGame env m | env -> m

createPlayer :: MonadGame env m => CardCode -> m ()
createPlayer code = do
  ident <- getRandom
  case lookupAlterEgo code ident of
    Nothing -> throwM (MissingCardCode "createPlayer" code)
    Just character -> addPlayer $ newPlayer character

getGame :: MonadGame env m => m Game
getGame = readIORef =<< asks (view gameL)

runGameMessages :: MonadGame env m => m ()
runGameMessages = do
  mMsg <- pop
  case mMsg of
    Nothing -> pure ()
    Just msg -> case msg of
      Ask ident choices -> do
        ref <- asks $ view gameL
        game <- readIORef ref
        atomicWriteIORef ref
          $ game { gameQuestion = fromList [(ident, choices)] }
      other -> do
        ref <- asks $ view gameL
        game <- readIORef ref
        game' <- runMessage other game
        atomicWriteIORef ref game'
        runGameMessages
