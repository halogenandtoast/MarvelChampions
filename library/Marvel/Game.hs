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
  }
  deriving stock Show

instance RunMessage Game where
  runMessage g = \case
    Ask{} -> pure g
    StartGame -> do
      case gamePlayers g of
        [] -> throwM NoPlayers
        players@(p : _) -> chooseOrRunOne
          (toId p)
          (map
            (\x -> cardLabel
              x
              [ ChoosePlayerOrder
                  (toId p)
                  (Unsorted $ filter (/= x) players)
                  (Sorted [x])
              ]
            )
            players
          )
      pure $ g { gameState = InProgress }
    ChoosePlayerOrder _ (Unsorted []) (Sorted ys) ->
      pure $ g { gamePlayers = ys }
    ChoosePlayerOrder _ (Unsorted [x]) (Sorted ys) ->
      pure $ g { gamePlayers = ys ++ [x] }
    ChoosePlayerOrder ident (Unsorted xs) (Sorted ys) -> do
      chooseOrRunOne
        ident
        (map
          (\x -> cardLabel
            x
            [ ChoosePlayerOrder
                ident
                (Unsorted $ filter (/= x) xs)
                (Sorted $ ys <> [x])
            ]
          )
          xs
        )
      pure g

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

class HasGame a where
  gameL :: Lens' a (IORef Game)

-- | Create a new game
-- Breaking down setup:
-- 1. Select Identities: This should be done and provided via the frontend
-- 2. Set Hit Points: Use startingHP from Identity
-- 3. Select First Player: Handle via Game Messages
newGame :: Game
newGame = Game PlayerPhase Unstarted mempty mempty

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
        game' <- runMessage game other
        atomicWriteIORef ref game'
        runGameMessages
