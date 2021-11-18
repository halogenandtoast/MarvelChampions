module Marvel.Game where

import Marvel.Prelude

import Marvel.Ability.Type
import {-# SOURCE #-} Marvel.Card.PlayerCard
import Marvel.Debug
import Marvel.Id
import Marvel.Matchers
import {-# SOURCE #-} Marvel.Modifier
import {-# SOURCE #-} Marvel.Queue
import Marvel.Resource
import Marvel.Source
import Marvel.Target

class HasGame a

class
  ( MonadCatch m
  , MonadThrow m
  , MonadIO m
  , MonadReader env m
  , HasGame env
  , HasQueue env
  , HasDebugLogger env
  , MonadRandom m
  )
  => MonadGame env m | env -> m, m -> env

data Game

getPlayers :: MonadGame env m => m [IdentityId]
getPlayerCount :: MonadGame env m => m Int
getsGame :: MonadGame env m => (Game -> a) -> m a
getUsedAbilities :: MonadGame env m => m (HashMap IdentityId [Ability])

-- Matchers
gameSelectIdentity
  :: MonadGame env m => IdentityMatcher -> m (HashSet IdentityId)
gameSelectEnemy :: MonadGame env m => EnemyMatcher -> m (HashSet EnemyId)
gameSelectVillain :: MonadGame env m => VillainMatcher -> m (HashSet VillainId)
gameSelectMinion :: MonadGame env m => MinionMatcher -> m (HashSet MinionId)
gameSelectAlly :: MonadGame env m => AllyMatcher -> m (HashSet AllyId)
gameSelectSupport :: MonadGame env m => SupportMatcher -> m (HashSet SupportId)
gameSelectScheme :: MonadGame env m => SchemeMatcher -> m (HashSet SchemeId)

instance HasAbilities Game

getAvailableResourcesFor :: MonadGame env m => PlayerCard -> m [Resource]

getModifiers :: (MonadGame env m, IsSource a, IsTarget a) => a -> m [Modifier]
