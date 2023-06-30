module Marvel.Game where

import Marvel.Prelude

import Control.Monad.IO.Class
import Marvel.Ability.Types
import {-# SOURCE #-} Marvel.Card.EncounterCard
import Marvel.Card.PlayerCard.Types
import Marvel.Count
import Marvel.Debug
import Marvel.Difficulty
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers.Types
import Marvel.Modifier.Types
import Marvel.Payment.Types
import Marvel.Projection
import Marvel.Queue
import Marvel.Ref
import Marvel.Resource.Types
import Marvel.Window.Types

class (Monad m) => HasGame m where
  getGame :: m Game

class HasGameRef m where
  getGameRef :: m (IORef Game)

class
  ( MonadCatch m
  , MonadThrow m
  , MonadIO m
  , MonadReader env m
  , HasGame m
  , HasGameRef m
  , HasQueue m
  , HasDebugLogger env
  , MonadRandom m
  , CoerceRole m
  ) =>
  MonadGame env m
    | env -> m
    , m -> env

data Game

getPlayers :: (HasGame m) => m [IdentityId]
getActivePlayerId :: (HasGame m) => m IdentityId
getPlayerCount :: (HasGame m) => m Int
getsGame :: (HasGame m) => (Game -> a) -> m a
getUsedAbilities :: (HasGame m) => m (HashMap IdentityId [Ability])
-- Matchers
gameSelectIdentity :: (HasGame m) => IdentityMatcher -> m (HashSet IdentityId)
gameSelectEnemy :: (HasGame m) => EnemyMatcher -> m (HashSet EnemyId)
gameSelectVillain :: (HasGame m) => VillainMatcher -> m (HashSet VillainId)
gameSelectMinion :: (HasGame m) => MinionMatcher -> m (HashSet MinionId)
gameSelectAlly :: (HasGame m) => AllyMatcher -> m (HashSet AllyId)
gameSelectSupport :: (HasGame m) => SupportMatcher -> m (HashSet SupportId)
gameSelectUpgrade :: (HasGame m) => UpgradeMatcher -> m (HashSet UpgradeId)
gameSelectAttachment ::
  (HasGame m) => AttachmentMatcher -> m (HashSet AttachmentId)
gameSelectScheme :: (HasGame m) => SchemeMatcher -> m (HashSet SchemeId)
gameSelectSideScheme ::
  (HasGame m) => SideSchemeMatcher -> m (HashSet SideSchemeId)
gameSelectTreachery :: (HasGame m) => TreacheryMatcher -> m (HashSet TreacheryId)
gameSelectCharacter :: (HasGame m) => CharacterMatcher -> m (HashSet CharacterId)
gameSelectExtendedCard ::
  ( MonadThrow m
  , HasGame m
  , HasQueue m
  , Projection m PlayerIdentity
  ) =>
  ExtendedCardMatcher ->
  m (HashSet PlayerCard)
gameSelectEncounterCard ::
  (MonadRandom m, HasGame m) =>
  EncounterCardMatcher ->
  m (HashSet EncounterCard)

instance HasAbilities Game

getAvailableResourcesFor ::
  ( HasCallStack
  , HasQueue m
  , MonadRandom m
  , MonadThrow m
  , HasGame m
  , Projection m PlayerIdentity
  ) =>
  Maybe PlayerCard ->
  m [Resource]
getModifiers :: (HasGame m, IsRef a) => a -> m [Modifier]
getCurrentWindows :: (HasGame m) => m [Window]
getCurrentPayment :: (HasGame m) => m Payment
getActiveCost :: (HasGame m) => m (Maybe ActiveCostId)
getDifficulty :: (HasGame m) => m Difficulty
getHazardCount :: (HasGame m) => m Natural
getAccelerationCount :: (HasGame m) => m Natural
getAvailablePaymentSources :: (HasGame m) => m [PlayerCard]
getResourceAbilities ::
  ( MonadThrow m
  , MonadRandom m
  , HasQueue m
  , HasCallStack
  , HasGame m
  , Projection m PlayerIdentity
  ) =>
  m [Ability]
gameSelectCountScheme ::
  (HasGame m) => QueryCount SchemeMatcher -> SchemeMatcher -> m Natural
gameSelectCountIdentity ::
  (HasGame m) => QueryCount IdentityMatcher -> IdentityMatcher -> m Natural
