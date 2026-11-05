module Marvel.Game.Select where

import {-# SOURCE #-} Marvel.Card.EncounterCard (EncounterCard)
import Marvel.Count (QueryCount)
import Marvel.Game.Class (HasGame)
import Marvel.Id
import Marvel.Matchers.Types
import Marvel.Prelude

gameSelectIdentity :: (HasGame m) => IdentityMatcher -> m (HashSet IdentityId)
gameSelectEnemy :: (HasGame m) => EnemyMatcher -> m (HashSet EnemyId)
gameSelectVillain :: (HasGame m) => VillainMatcher -> m (HashSet VillainId)
gameSelectMinion :: (HasGame m) => MinionMatcher -> m (HashSet MinionId)
gameSelectAlly :: (HasGame m) => AllyMatcher -> m (HashSet AllyId)
gameSelectSupport :: (HasGame m) => SupportMatcher -> m (HashSet SupportId)
gameSelectUpgrade :: (HasGame m) => UpgradeMatcher -> m (HashSet UpgradeId)
gameSelectAttachment ::
  (HasGame m) => AttachmentMatcher -> m (HashSet AttachmentId)
gameSelectEnvironment ::
  (HasGame m) => EnvironmentMatcher -> m (HashSet EnvironmentId)
gameSelectScheme :: (HasGame m) => SchemeMatcher -> m (HashSet SchemeId)
gameSelectSideScheme ::
  (HasGame m) => SideSchemeMatcher -> m (HashSet SideSchemeId)
gameSelectTreachery :: (HasGame m) => TreacheryMatcher -> m (HashSet TreacheryId)
gameSelectCharacter :: (HasGame m) => CharacterMatcher -> m (HashSet CharacterId)
gameSelectEncounterCard ::
  (MonadRandom m, HasGame m) =>
  EncounterCardMatcher ->
  m (HashSet EncounterCard)
gameSelectCountScheme ::
  (HasGame m) => QueryCount SchemeMatcher -> SchemeMatcher -> m Natural
gameSelectCountIdentity ::
  (HasGame m) => QueryCount IdentityMatcher -> IdentityMatcher -> m Natural
gameSelectCountAlly ::
  (HasGame m) => QueryCount AllyMatcher -> AllyMatcher -> m Natural

-- Hela
