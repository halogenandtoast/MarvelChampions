module Marvel.Game where

import Marvel.Prelude

import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT)
import Marvel.Ability.Types
import {-# SOURCE #-} Marvel.Card.EncounterCard
import {-# SOURCE #-} Marvel.Card.Def (CardDef)
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
import Marvel.Trait.Types (Trait)
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

instance (Monad m) => HasGame (ReaderT Game m)
instance (HasQueue m) => HasQueue (ReaderT Game m)
instance (HasGame m) => Projection m PlayerIdentity

getPlayers :: (HasGame m) => m [IdentityId]
getActivePlayerId :: (HasGame m) => m IdentityId
getYou :: (HasGame m) => m IdentityId
asYou :: (HasGame m) => IdentityId -> ReaderT Game m a -> m a
getPlayerCount :: (HasGame m) => m Int
getsGame :: (HasGame m) => (Game -> a) -> m a
abilityCardDef :: Game -> Target -> Maybe CardDef
getUsedAbilities :: (HasGame m) => m (HashMap IdentityId [Ability])
getPlayedThisRound :: (HasGame m) => m [(IdentityId, CardDef)]
getEncounterDeck :: (HasGame m) => m [EncounterCard]
getAbilitiesFor :: (HasGame m) => IdentityId -> m [Ability]
getSourceController :: (HasGame m) => Source -> m (Maybe IdentityId)
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
gameSelectEnvironment ::
  (HasGame m) => EnvironmentMatcher -> m (HashSet EnvironmentId)
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
  IdentityId ->
  Maybe PlayerCard ->
  m [Resource]
getModifiers :: (HasGame m, IsRef a) => a -> m [Modifier]
getUpgradeHeldCards :: (HasGame m) => IdentityId -> m [PlayerCard]
getPlayTargets ::
  (HasGame m, HasQueue m, MonadThrow m, Projection m PlayerIdentity) =>
  IdentityId ->
  PlayerCard ->
  m (Maybe [Target])
getCurrentWindows :: (HasGame m) => m [Window]
getCurrentPayment :: (HasGame m) => m Payment
getActiveCost :: (HasGame m) => m (Maybe ActiveCostId)
getDifficulty :: (HasGame m) => m Difficulty
getUniqueTitlesInPlay :: (HasGame m) => m [(Text, [Text])]
getVillainActivationPlayer :: (HasGame m) => VillainId -> m (Maybe IdentityId)
countCardsInPlayWithTrait :: (HasGame m) => Trait -> m Natural
getHazardCount :: (HasGame m) => m Natural
getAccelerationCount :: (HasGame m) => m Natural
getAvailablePaymentSources ::
  (HasQueue m, MonadThrow m, HasGame m, Projection m PlayerIdentity) => IdentityId -> m [PlayerCard]
getResourceAbilities ::
  ( MonadThrow m
  , MonadRandom m
  , HasQueue m
  , HasGame m
  ) =>
  IdentityId ->
  Maybe PlayerCard ->
  m [Ability]
abilityResources ::
  (HasQueue m, MonadThrow m, HasGame m) => Ability -> m [Resource]
gameSelectCountScheme ::
  (HasGame m) => QueryCount SchemeMatcher -> SchemeMatcher -> m Natural
gameSelectCountIdentity ::
  (HasGame m) => QueryCount IdentityMatcher -> IdentityMatcher -> m Natural
gameSelectCountAlly ::
  (HasGame m) => QueryCount AllyMatcher -> AllyMatcher -> m Natural
