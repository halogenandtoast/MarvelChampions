{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Api.Marvel.Helpers where

import Import hiding (appLogger)

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.Random (MonadRandom (..))
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Num (Natural)
import Marvel.Ally
import Marvel.Attachment.Types
import Marvel.Card hiding (toCard)
import Marvel.Deck
import Marvel.Discard
import Marvel.Entity (Id)
import Marvel.Game
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Identity
import Marvel.MainScheme.Types
import Marvel.Minion.Types
import Marvel.PlayerCard
import Marvel.Question
import Marvel.Scenario.Types
import Marvel.SideScheme.Types
import Marvel.Support.Types
import Marvel.Upgrade.Types
import Marvel.Villain.Types

data ApiGame = ApiGame
  { id :: Key MarvelGame
  , name :: Text
  , players :: HashMap (Id PlayerIdentity) ApiPlayerIdentity
  , villains :: HashMap (Id Villain) Villain
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
  deriving anyclass (ToJSON)

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
  deriving anyclass (ToJSON)

toApiPlayer :: (MonadGame env m) => PlayerIdentity -> m ApiPlayerIdentity
toApiPlayer i@(PlayerIdentity (PlayerIdentityAttrs {..})) = do
  modifiedHp <- getModifiedHp i
  pure $
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
      , hp = modifiedHp
      , damage = playerIdentityDamage
      , encounterCards = playerIdentityEncounterCards
      }

toInactiveApiPlayer :: PlayerIdentity -> ApiPlayerIdentity
toInactiveApiPlayer (PlayerIdentity (PlayerIdentityAttrs {..})) =
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

toApiGame :: (MonadGame env m) => Entity MarvelGame -> m ApiGame
toApiGame (Entity gameId MarvelGame {marvelGameCurrentData, marvelGameName}) =
  do
    let g@Game {..} = marvelGameCurrentData
    modifiedPlayers <- HashMap.fromList <$> traverse (\(i, p) -> (i,) <$> toApiPlayer p) (HashMap.toList $ gamePlayers g)
    pure $
      ApiGame
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
toInactiveApiGame (Entity gameId MarvelGame {marvelGameCurrentData, marvelGameName}) =
  let g@Game {..} = marvelGameCurrentData
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
  deriving stock (Generic)
  deriving anyclass (ToJSON)

noLogger :: (Applicative m) => Text -> m ()
noLogger = const (pure ())

findRoom :: MarvelGameId -> TVar (Map.Map MarvelGameId Room) -> STM (Maybe Room)
findRoom gameId roomsRef = do
  rooms <- readTVar roomsRef
  pure $ Map.lookup gameId rooms

getChannel :: MarvelGameId -> Handler (TChan BSL.ByteString)
getChannel gameId = do
  roomsRef <- appGameRooms <$> getYesod
  liftIO $ atomically $ getChannelSTM gameId roomsRef

getChannelSTM :: MarvelGameId -> TVar (Map.Map MarvelGameId Room) -> STM (TChan BSL.ByteString)
getChannelSTM gameId roomsRef = do
  mRoom <- findRoom gameId roomsRef
  case mRoom of
    Just room -> pure $ roomChan room
    Nothing -> do
      chan <- newBroadcastTChan
      clients <- newTVar 0
      let room = Room gameId clients chan
      modifyTVar' roomsRef $ Map.insert gameId room
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
loadDecklist marvelDeck = (heroCardCode,) <$> toDeck decklist
 where
  decklist = marvelDeckList marvelDeck
  heroCardCode = investigator_code decklist
