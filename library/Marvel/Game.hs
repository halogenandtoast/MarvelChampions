module Marvel.Game where

import Marvel.Prelude

import Control.Monad.Catch
import Marvel.AlterEgo.Cards
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Exception
import Marvel.Identity
import Marvel.Message
import Marvel.Player
import Marvel.Queue
import Marvel.Scenario
import Marvel.Villain
import Marvel.Villain.Attrs

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

runGameMessage :: MonadGame env m => Message -> Game -> m Game
runGameMessage msg g = case msg of
  StartGame -> do
    push StartScenario
    case gamePlayers g of
      [] -> throwM NoPlayers
      players@(p : _) -> choosePlayerOrder (toId p) players
    pure $ g { gameState = InProgress }
  SetPlayerOrder xs -> pure $ g { gamePlayers = xs }
  AddVillain cardCode -> do
    villainId <- getRandom
    case lookupVillain cardCode villainId of
      Just x -> pure $ g & villainsL . at villainId ?~ x
      Nothing -> throwM $ MissingCardCode "AddVillain" cardCode
  _ -> pure g

instance RunMessage Game where
  runMessage msg g =
    traverseOf scenarioL (runMessage msg) g >>= runGameMessage msg

chooseOne :: MonadGame env m => IdentityId -> [Choice] -> m ()
chooseOne ident msgs = push (Ask ident $ ChooseOne msgs)

chooseOrRunOne :: MonadGame env m => IdentityId -> [Choice] -> m ()
chooseOrRunOne ident = \case
  [] -> throwM NoChoices
  [choice] -> pushAll $ choiceMessages choice
  choices -> push (Ask ident $ ChooseOne choices)

choosePlayerOrder :: MonadGame env m => IdentityId -> [Player] -> m ()
choosePlayerOrder ident xs =
  push (Ask ident $ ChoosePlayerOrder (Unsorted xs) mempty)

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
withGame f = do
  game <- asks $ view gameL
  atomicModifyIORef' game f

withGame_ :: MonadGame env m => (Game -> Game) -> m ()
withGame_ f = withGame ((, ()) . f)

withGameM :: MonadGame env m => (Game -> m Game) -> m ()
withGameM f = getGame >>= f >>= withGame_ . const

getsGame :: MonadGame env m => (Game -> a) -> m a
getsGame f = withGame (id &&& f)

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
createPlayer cardCode = do
  ident <- getRandom
  let
    mAlterEgo = do
      def <- lookup cardCode allAlterEgosMap
      lookupAlterEgo def ident
  maybe missingCardCode (addPlayer . newPlayer) mAlterEgo
  where missingCardCode = throwM (MissingCardCode "createPlayer" cardCode)

getGame :: MonadGame env m => m Game
getGame = readIORef =<< asks (view gameL)

runGameMessages :: MonadGame env m => m ()
runGameMessages = do
  mMsg <- pop
  case mMsg of
    Nothing -> pure ()
    Just msg -> case msg of
      Ask ident choices -> do
        withGame_ $ questionL .~ fromList [(ident, choices)]
      other -> do
        withGameM $ runMessage other
        runGameMessages
