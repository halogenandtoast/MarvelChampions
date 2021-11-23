module Marvel.Query where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Marvel.Card.PlayerCard
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers

type family QueryElement a where
  QueryElement IdentityMatcher = IdentityId
  QueryElement EnemyMatcher = EnemyId
  QueryElement VillainMatcher = VillainId
  QueryElement MinionMatcher = MinionId
  QueryElement AllyMatcher = AllyId
  QueryElement SupportMatcher = SupportId
  QueryElement SchemeMatcher = SchemeId
  QueryElement SideSchemeMatcher = SideSchemeId
  QueryElement UpgradeMatcher = UpgradeId
  QueryElement CharacterMatcher = CharacterId
  QueryElement ExtendedCardMatcher = PlayerCard
  QueryElement AttachmentMatcher = AttachmentId

class Query a where
  select :: MonadGame env m => a -> m (HashSet (QueryElement a))

selectList :: (MonadGame env m, Query a) => a -> m [QueryElement a]
selectList = fmap HashSet.toList . select

selectAny :: (MonadGame env m, Query a) => a -> m Bool
selectAny = fmap (not . null) . select

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

instance Query SupportMatcher where
  select = gameSelectSupport

instance Query MinionMatcher where
  select = gameSelectMinion

instance Query UpgradeMatcher where
  select = gameSelectUpgrade

instance Query CharacterMatcher where
  select = gameSelectCharacter

instance Query ExtendedCardMatcher where
  select = gameSelectExtendedCard

instance Query AttachmentMatcher where
  select = gameSelectAttachment

instance Query SideSchemeMatcher where
  select = gameSelectSideScheme
