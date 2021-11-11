module Marvel.Query where

import Marvel.Prelude

import qualified Data.HashSet as HashSet
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers

type family QueryElement a where
  QueryElement IdentityMatcher = IdentityId
  QueryElement EnemyMatcher = EnemyId
  QueryElement VillainMatcher = VillainId
  QueryElement AllyMatcher = AllyId
  QueryElement SchemeMatcher = SchemeId

class Query a where
  select :: MonadGame env m => a -> m (HashSet (QueryElement a))

selectList :: (MonadGame env m, Query a) => a -> m [QueryElement a]
selectList = fmap HashSet.toList . select

selectOne :: (MonadGame env m, Query a) => a -> m (Maybe (QueryElement a))
selectOne matcher = do
  result <- selectList matcher
  pure $ case result of
    [] -> Nothing
    x : _ -> Just x

selectJust :: (MonadGame env m, Query a) => a -> m (QueryElement a)
selectJust matcher = do
  result <- selectOne matcher
  case result of
    Nothing -> error "Must guarantee a result"
    Just x -> pure x

instance Query IdentityMatcher where
  select = gameSelectIdentity

instance Query EnemyMatcher where
  select = gameSelectEnemy

instance Query VillainMatcher where
  select = gameSelectVillain

instance Query SchemeMatcher where
  select = gameSelectScheme

instance Query AllyMatcher where
  select = gameSelectAlly
