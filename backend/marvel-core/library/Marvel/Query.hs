module Marvel.Query where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Marvel.Card.EncounterCard
import Marvel.Card.PlayerCard
import Marvel.Game.Source
import Marvel.Id
import {-# SOURCE #-} Marvel.Matchers

class Query a where
  type QueryElement a
  select :: MonadGame env m => a -> m (HashSet (QueryElement a))

selectList :: (MonadGame env m, Query a) => a -> m [QueryElement a]
selectList = fmap HashSet.toList . select

selectMap :: (MonadGame env m, Query a) => (QueryElement a -> b) -> a -> m [b]
selectMap f = fmap (map f . HashSet.toList) . select

selectAny :: (MonadGame env m, Query a) => a -> m Bool
selectAny = fmap (not . null) . select

selectListCount :: (MonadGame env m, Query a) => a -> m Natural
selectListCount = fmap (fromIntegral . length) . selectList

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
  type QueryElement IdentityMatcher = IdentityId
  select = gameSelectIdentity

instance Query EnemyMatcher where
  type QueryElement EnemyMatcher = EnemyId
  select = gameSelectEnemy

instance Query VillainMatcher where
  type QueryElement VillainMatcher = VillainId
  select = gameSelectVillain

instance Query SchemeMatcher where
  type QueryElement SchemeMatcher = SchemeId
  select = gameSelectScheme

instance Query AllyMatcher where
  type QueryElement AllyMatcher = AllyId
  select = gameSelectAlly

instance Query SupportMatcher where
  type QueryElement SupportMatcher = SupportId
  select = gameSelectSupport

instance Query MinionMatcher where
  type QueryElement MinionMatcher = MinionId
  select = gameSelectMinion

instance Query UpgradeMatcher where
  type QueryElement UpgradeMatcher = UpgradeId
  select = gameSelectUpgrade

instance Query CharacterMatcher where
  type QueryElement CharacterMatcher = CharacterId
  select = gameSelectCharacter

instance Query ExtendedCardMatcher where
  type QueryElement ExtendedCardMatcher = PlayerCard
  select = gameSelectExtendedCard

instance Query AttachmentMatcher where
  type QueryElement AttachmentMatcher = AttachmentId
  select = gameSelectAttachment

instance Query SideSchemeMatcher where
  type QueryElement SideSchemeMatcher = SideSchemeId
  select = gameSelectSideScheme

instance Query EncounterCardMatcher where
  type QueryElement EncounterCardMatcher = EncounterCard
  select = gameSelectEncounterCard
