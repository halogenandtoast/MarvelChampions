module Marvel.Game where

import Marvel.Prelude

import Marvel.Ability.Type
import {-# SOURCE #-} Marvel.Card.PlayerCard
import Marvel.Debug
import Marvel.Difficulty
import Marvel.Id
import {-# SOURCE #-} Marvel.Matchers
import {-# SOURCE #-} Marvel.Modifier
import {-# SOURCE #-} Marvel.Queue
import Marvel.Resource
import Marvel.Source
import Marvel.Target
import {-# SOURCE #-} Marvel.Window

class HasGame a

class
  ( MonadCatch m , MonadThrow m
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
gameSelectUpgrade :: MonadGame env m => UpgradeMatcher -> m (HashSet UpgradeId)
gameSelectScheme :: MonadGame env m => SchemeMatcher -> m (HashSet SchemeId)
gameSelectTreachery
  :: MonadGame env m => TreacheryMatcher -> m (HashSet TreacheryId)
gameSelectCharacter
  :: MonadGame env m => CharacterMatcher -> m (HashSet CharacterId)
gameSelectExtendedCard
  :: MonadGame env m => ExtendedCardMatcher -> m (HashSet PlayerCard)

instance HasAbilities Game

getAvailableResourcesFor :: MonadGame env m => Maybe PlayerCard -> m [Resource]

getModifiers :: (MonadGame env m, IsSource a, IsTarget a) => a -> m [Modifier]
getCurrentWindows :: MonadGame env m => m [Window]
getDifficulty :: MonadGame env m => m Difficulty
