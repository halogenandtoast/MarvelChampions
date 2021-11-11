module Marvel.Query where

import Marvel.Prelude

import qualified Data.HashSet as HashSet
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers

type family QueryElement a where
  QueryElement IdentityMatcher = IdentityId
  QueryElement EnemyMatcher = EnemyId
  QueryElement AllyMatcher = AllyId
  QueryElement SchemeMatcher = SchemeId

class Query a where
  select :: MonadGame env m => a -> m (HashSet (QueryElement a))

selectList :: (MonadGame env m, Query a) => a -> m [QueryElement a]
selectList = fmap HashSet.toList . select

instance Query IdentityMatcher where
  select = gameSelectIdentity

instance Query EnemyMatcher where
  select = gameSelectEnemy

instance Query SchemeMatcher where
  select = gameSelectScheme

instance Query AllyMatcher where
  select = gameSelectAlly
