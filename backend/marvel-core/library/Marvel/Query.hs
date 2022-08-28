module Marvel.Query where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Marvel.Card.EncounterCard
import Marvel.Card.PlayerCard
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers.Types
import Marvel.Queue
import Marvel.Projection
import Marvel.Identity.Types

class HasGame m => Query m a where
  type QueryElement a
  select :: a -> m (HashSet (QueryElement a))

match
  :: (Query m a, Hashable (QueryElement a)) => QueryElement a -> a -> m Bool
match a matcher = member a <$> select matcher

selectList :: Query m a => a -> m [QueryElement a]
selectList = fmap HashSet.toList . select

selectMap :: Query m a => (QueryElement a -> b) -> a -> m [b]
selectMap f = fmap (map f . HashSet.toList) . select

selectAny :: Query m a => a -> m Bool
selectAny = fmap (not . null) . select

selectListCount :: Query m a => a -> m Natural
selectListCount = fmap (fromIntegral . length) . selectList

selectOne :: Query m a => a -> m (Maybe (QueryElement a))
selectOne matcher = do
  result <- selectList matcher
  pure $ case result of
    [] -> Nothing
    x : _ -> Just x

selectJust :: Query m a => a -> m (QueryElement a)
selectJust matcher = do
  result <- selectOne matcher
  case result of
    Nothing -> error "Must guarantee a result"
    Just x -> pure x

instance HasGame m => Query m IdentityMatcher where
  type QueryElement IdentityMatcher = IdentityId
  select = gameSelectIdentity

instance HasGame m => Query m EnemyMatcher where
  type QueryElement EnemyMatcher = EnemyId
  select = gameSelectEnemy

instance HasGame m => Query m VillainMatcher where
  type QueryElement VillainMatcher = VillainId
  select = gameSelectVillain

instance HasGame m => Query m SchemeMatcher where
  type QueryElement SchemeMatcher = SchemeId
  select = gameSelectScheme

instance HasGame m => Query m AllyMatcher where
  type QueryElement AllyMatcher = AllyId
  select = gameSelectAlly

instance HasGame m => Query m SupportMatcher where
  type QueryElement SupportMatcher = SupportId
  select = gameSelectSupport

instance HasGame m => Query m MinionMatcher where
  type QueryElement MinionMatcher = MinionId
  select = gameSelectMinion

instance HasGame m => Query m UpgradeMatcher where
  type QueryElement UpgradeMatcher = UpgradeId
  select = gameSelectUpgrade

instance HasGame m => Query m CharacterMatcher where
  type QueryElement CharacterMatcher = CharacterId
  select = gameSelectCharacter

instance (MonadThrow m, HasGame m, HasQueue m, Projection m PlayerIdentity) => Query m ExtendedCardMatcher where
  type QueryElement ExtendedCardMatcher = PlayerCard
  select = gameSelectExtendedCard

instance HasGame m => Query m AttachmentMatcher where
  type QueryElement AttachmentMatcher = AttachmentId
  select = gameSelectAttachment

instance HasGame m => Query m SideSchemeMatcher where
  type QueryElement SideSchemeMatcher = SideSchemeId
  select = gameSelectSideScheme

instance (MonadRandom m, HasGame m) => Query m EncounterCardMatcher where
  type QueryElement EncounterCardMatcher = EncounterCard
  select = gameSelectEncounterCard
