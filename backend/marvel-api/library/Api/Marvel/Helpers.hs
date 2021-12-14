{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Api.Marvel.Helpers where

import Import hiding (appLogger)

import Control.Concurrent.STM.TChan
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Random (MonadRandom(..))
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Marvel.Ally
import Marvel.Attachment
import Marvel.Card hiding (toCard)
import Marvel.Debug
import Marvel.Deck
import Marvel.Discard
import Marvel.Entity (EntityId)
import Marvel.Game
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Identity
import Marvel.MainScheme
import Marvel.Message
import Marvel.Minion
import Marvel.PlayerCard
import Marvel.Question
import Marvel.Queue
import Marvel.Scenario
import Marvel.SideScheme
import Marvel.Support
import Marvel.Upgrade
import Marvel.Villain

data ApiGame = ApiGame
  { id :: Key MarvelGame
  , name :: Text
  , players :: HashMap (EntityId PlayerIdentity) ApiPlayerIdentity
  , villains :: HashMap (EntityId Villain) Villain
  , scenario :: Scenario
  , question :: HashMap IdentityId Question
  , allies :: HashMap AllyId Ally
  , minions :: HashMap MinionId Minion
  , attachments :: HashMap AttachmentId Attachment
  , supports :: HashMap SupportId Support
  , upgrades :: HashMap UpgradeId Upgrade
  , sideSchemes :: HashMap SideSchemeId SideScheme
  , mainSchemes :: HashMap MainSchemeId MainScheme
  , state :: GameState
  , focusedCards :: [Card]
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data ApiPlayerIdentity = ApiPlayerIdentity
  { id :: IdentityId
  , hand :: [PlayerCard]
  , discard :: [PlayerCard]
  , side :: Side
  , sides :: HashMap Side PlayerIdentitySide
  , allies :: HashSet AllyId
  , minions :: HashSet MinionId
  , supports :: HashSet SupportId
  , upgrades :: HashSet UpgradeId
  , exhausted :: Bool
  , stunned :: Bool
  , confused :: Bool
  , tough :: Bool
  , hp :: Natural
  , damage :: Natural
  , encounterCards :: [EncounterCard]
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

toApiPlayer :: MonadGame env m => PlayerIdentity -> m ApiPlayerIdentity
toApiPlayer i@PlayerIdentity {..} = do
  modifiedHp <- getModifiedHp i
  pure $ ApiPlayerIdentity
    { id = playerIdentityId
    , hand = unHand playerIdentityHand
    , discard = unDiscard playerIdentityDiscard
    , side = playerIdentitySide
    , sides = playerIdentitySides
    , allies = playerIdentityAllies
    , minions = playerIdentityMinions
    , supports = playerIdentitySupports
    , upgrades = playerIdentityUpgrades
    , exhausted = playerIdentityExhausted
    , stunned = playerIdentityStunned
    , confused = playerIdentityConfused
    , tough = playerIdentityTough
    , hp = modifiedHp
    , damage = playerIdentityDamage
    , encounterCards = playerIdentityEncounterCards
    }

toInactiveApiPlayer :: PlayerIdentity -> ApiPlayerIdentity
toInactiveApiPlayer PlayerIdentity {..} =
  ApiPlayerIdentity
    { id = playerIdentityId
    , hand = unHand playerIdentityHand
    , discard = unDiscard playerIdentityDiscard
    , side = playerIdentitySide
    , sides = playerIdentitySides
    , allies = playerIdentityAllies
    , minions = playerIdentityMinions
    , supports = playerIdentitySupports
    , upgrades = playerIdentityUpgrades
    , exhausted = playerIdentityExhausted
    , stunned = playerIdentityStunned
    , confused = playerIdentityConfused
    , tough = playerIdentityTough
    , hp = unHp playerIdentityHP
    , damage = playerIdentityDamage
    , encounterCards = playerIdentityEncounterCards
    }

toApiGame :: MonadGame env m => Entity MarvelGame -> m ApiGame
toApiGame (Entity gameId MarvelGame { marvelGameCurrentData, marvelGameName })
  = do
    let g@Game {..} = marvelGameCurrentData
    modifiedPlayers <- HashMap.fromList <$> traverse (\(i, p) -> (i,) <$> toApiPlayer p) (HashMap.toList $ gamePlayers g)
    pure $ ApiGame
      { id = gameId
      , name = marvelGameName
      , question = gameQuestion
      , scenario = gameScenario
      , players = modifiedPlayers
      , villains = gameVillains g
      , allies = gameAllies g
      , minions = gameMinions g
      , attachments = gameAttachments g
      , supports = gameSupports g
      , upgrades = gameUpgrades g
      , sideSchemes = gameSideSchemes g
      , mainSchemes = gameMainSchemes g
      , state = gameState
      , focusedCards = gameFocusedCards
      }

toInactiveApiGame :: Entity MarvelGame -> ApiGame
toInactiveApiGame (Entity gameId MarvelGame { marvelGameCurrentData, marvelGameName })
  = let g@Game {..} = marvelGameCurrentData
      in ApiGame
        { id = gameId
        , name = marvelGameName
        , question = gameQuestion
        , scenario = gameScenario
        , players = HashMap.map toInactiveApiPlayer $ gamePlayers g
        , villains = gameVillains g
        , allies = gameAllies g
        , minions = gameMinions g
        , attachments = gameAttachments g
        , supports = gameSupports g
        , upgrades = gameUpgrades g
        , sideSchemes = gameSideSchemes g
        , mainSchemes = gameMainSchemes g
        , state = gameState
        , focusedCards = gameFocusedCards
        }

data ApiResponse = GameUpdate ApiGame | GameMessage Text
  deriving stock Generic
  deriving anyclass ToJSON

newtype GameAppT a = GameAppT { unGameAppT :: ReaderT GameApp IO a }
  deriving newtype (MonadReader GameApp, Functor, Applicative, Monad, MonadFail, MonadIO, MonadRandom, MonadCatch, MonadThrow)

data GameApp = GameApp
  { appGame :: IORef Game
  , appQueue :: IORef [Message]
  , appDebugLogger :: Maybe DebugLogger
  }

newApp :: MonadIO m => Game -> [Message] -> m GameApp
newApp g msgs = do
  gameRef <- newIORef g
  queueRef <- newIORef msgs
  pure $ GameApp gameRef queueRef Nothing

instance HasGame GameApp where
  game = appGame

instance HasQueue GameApp where
  queue = appQueue

instance HasDebugLogger GameApp where
  debugLogger = appDebugLogger

instance MonadGame GameApp GameAppT

runGameApp :: MonadIO m => GameApp -> GameAppT a -> m a
runGameApp gameApp = liftIO . flip runReaderT gameApp . unGameAppT

noLogger :: Applicative m => Text -> m ()
noLogger = const (pure ())

getChannel :: MarvelGameId -> Handler (TChan BSL.ByteString)
getChannel gameId = do
  gameChannelsRef <- appGameChannels <$> getYesod
  gameChannels <- readIORef gameChannelsRef
  case Map.lookup gameId gameChannels of
    Just chan -> pure chan
    Nothing -> do
      chan <- atomically newBroadcastTChan
      atomicModifyIORef' gameChannelsRef
        $ \gameChannels' -> (Map.insert gameId chan gameChannels', ())
      pure chan

toDeck :: MarvelDBDecklist -> IO Deck
toDeck =
  fmap Deck
    . traverse toCard
    . concatMap (uncurry (flip replicate))
    . Map.toList
    . slots

toCard :: CardCode -> IO PlayerCard
toCard code = do
  cardId <- getRandom
  pure $ MkPlayerCard cardId (lookupPlayerCard code) Nothing Nothing

loadDecklist :: MarvelDeck -> IO (CardCode, Deck)
loadDecklist marvelDeck = (heroCardCode, ) <$> toDeck decklist
 where
  decklist = marvelDeckList marvelDeck
  heroCardCode = investigator_code decklist
