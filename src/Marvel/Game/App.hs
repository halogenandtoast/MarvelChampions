module Marvel.Game.App where

import Control.Monad.Reader (MonadIO (..), ReaderT (..), asks)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Tuple (swap)
import Marvel.Debug (DebugLogger, HasDebugLogger (..))
import Marvel.Frontend (Frontend (..), FrontendMessage)
import Marvel.Game.Base (Game (..), DiscardDisplay (..), gamePlayers)
import Data.HashMap.Strict qualified as HashMap
import Data.List (nubBy)
import Marvel.Card
import Marvel.Ability.Types (Ability (..))
import Marvel.Question (Choice (..))
import Marvel.Window qualified as W
import Marvel.Discard (unDiscard)
import Marvel.Entity (toAttrs)
import Marvel.Identity.Types (Attrs (playerIdentityDiscard))
import Marvel.Scenario.Types (Attrs (scenarioDiscard))
import Marvel.Game.Class (HasGame (..), HasGameRef (..), MonadGame)
import Marvel.Message (Message (..), EventMessage (..))
import Marvel.Prelude
import Marvel.Queue (HasQueue (..))
import System.Random (
  StdGen,
  mkStdGen,
  random,
  randomR,
  randomRs,
  randoms,
  splitGen,
 )

newtype GameAppT a = GameAppT {unGameAppT :: ReaderT GameApp IO a}
  deriving newtype (MonadReader GameApp, Functor, Applicative, Monad, MonadFail, MonadIO, MonadCatch, MonadThrow)

{- | A game being played: the game and its queue, the random generator it rolls with
(stored with the game, so a reloaded game rolls the same), and where what it says to
its clients goes.
-}
data GameApp = GameApp
  { appGame :: IORef Game
  , appQueue :: IORef [Message]
  , appDebugLogger :: Maybe DebugLogger
  , appRandomGen :: IORef StdGen
  , appFrontend :: FrontendMessage -> IO ()
  }

instance MonadRandom GameAppT where
  getRandomR lohi = withGen (randomR lohi)
  getRandom = withGen random
  getRandomRs lohi = randomRs lohi <$> withGen splitGen
  getRandoms = randoms <$> withGen splitGen

withGen :: (StdGen -> (a, StdGen)) -> GameAppT a
withGen f = do
  ref <- asks appRandomGen
  liftIO $ atomicModifyIORef' ref (swap . f)

instance Frontend GameAppT where
  send msg = asks appFrontend >>= \f -> liftIO (f msg)

instance HasGame GameAppT where
  getGame = getGameRef >>= liftIO . readIORef

instance HasQueue GameAppT where
  getQueue = asks appQueue

instance HasDebugLogger GameApp where
  debugLogger = appDebugLogger

instance HasGameRef GameAppT where
  getGameRef = asks appGame

instance MonadGame GameApp GameAppT

newApp :: (MonadIO m) => Game -> [Message] -> m GameApp
newApp g msgs = newGameApp g msgs (mkStdGen 0) (const $ pure ())

{- | A game to play from its queue, rolling with the generator given and telling its
clients through the function given.
-}
newGameApp :: (MonadIO m) => Game -> [Message] -> StdGen -> (FrontendMessage -> IO ()) -> m GameApp
newGameApp g msgs gen frontend = liftIO do
  gameRef <- newIORef $ restoreResolvingEvents g msgs
  queueRef <- newIORef msgs
  genRef <- newIORef gen
  pure $ GameApp gameRef queueRef Nothing genRef frontend

-- | The game, its queue and its random generator as they now stand.
currentState :: GameApp -> IO (Game, [Message], StdGen)
currentState app = (,,) <$> readIORef (appGame app) <*> readIORef (appQueue app) <*> readIORef (appRandomGen app)

runGameApp :: (MonadIO m) => GameApp -> GameAppT a -> m a
runGameApp gameApp = liftIO . flip runReaderT gameApp . unGameAppT

-- | Old saves retained event entities after resolution. Their queued completion
-- messages identify the events still resolving, regardless of physical card zone.
restoreResolvingEvents :: Game -> [Message] -> Game
restoreResolvingEvents g messages = restored
  { gameDiscardDisplays = Just displays
  , gameBoostIconReplacements = Just $ fromMaybe recoveredReplacements (gameBoostIconReplacements g)
  }
 where
  restored = case gameResolvingEvents g of
    Just _ -> g
    Nothing -> g {gameResolvingEvents = Just $ concatMap pending messages}
  displays = fromMaybe inferred (gameDiscardDisplays g)
  -- Older saves kept the original callback key after Chaos Control. Its used
  -- ability records which count was replaced; the extra displayed discard is
  -- the physical card that supplied that replacement.
  recoveredReplacements =
    [ (ecCardId original, replacement)
    | uses <- HashMap.elems (gameUsedAbilities g)
    , (ability, _) <- uses
    , abilityLabel ability == Just "Chaos Control"
    , Run steps <- abilityChoices ability
    , CheckWindowsFor _ windows <- steps
    , W.Window _ (W.CountingBoostIcons original) <- windows
    , WithBoostIcons target _ counts <- messages
    , original `elem` map fst counts
    , frame <- displays
    , discardDisplaySource frame `elem` [Nothing, Just target]
    , let extras = [card | EncounterCard card <- discardDisplayCards frame, card `notElem` map fst counts]
    , replacement <- take 1 $ reverse extras
    ]
  inferred =
    [ DiscardDisplay (Just target) cards True
    | (target, candidates) <- concatMap awaitingCards messages
    , let cards = nubBy (\a b -> toCardId a == toCardId b) $ filter ((`elem` discardedIds) . toCardId) candidates
    , not (null cards)
    ]
  discardedIds = map ecCardId (scenarioDiscard $ toAttrs $ gameScenario g) <>
    [pcCardId card | player <- HashMap.elems (gamePlayers g), card <- unDiscard $ playerIdentityDiscard $ toAttrs player]
  awaitingCards = \case
    WithBoostIcons target _ cards -> [(target, map (EncounterCard . fst) cards)]
    WithDiscarded target _ cards -> [(target, cards)]
    WithDiscardedMatch target _ card -> [(target, [card])]
    Simultaneously nested -> concatMap awaitingCards nested
    _ -> []
  pending (EventMessage eid ResolvedEvent) = [eid]
  pending (Simultaneously messages') = concatMap pending messages'
  pending _ = []
