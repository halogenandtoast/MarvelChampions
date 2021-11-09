module Marvel.Game where

import Marvel.Prelude

import Marvel.Ability.Type
import {-# SOURCE #-} Marvel.Card.PlayerCard
import Marvel.Debug
import Marvel.Id
import Marvel.Matchers
import {-# SOURCE #-} Marvel.Queue
import Marvel.Resource

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

instance HasAbilities Game

getAvailableResourcesFor :: MonadGame env m => PlayerCard -> m [Resource]
