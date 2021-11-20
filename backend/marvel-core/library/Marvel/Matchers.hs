{-# LANGUAGE PatternSynonyms #-}
module Marvel.Matchers where

import Marvel.Prelude

import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id

data EntityMatcher = IdentityEntity IdentityMatcher | AllyEntity AllyMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data IdentityMatcher
  = HeroIdentity
  | AlterEgoIdentity
  | UnexhaustedIdentity
  | IdentityWithId IdentityId
  | AnyIdentity
  | IdentityWithDamage GameValueMatcher
  | You
  | IdentityMatchAll [IdentityMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Monoid IdentityMatcher where
  mempty = AnyIdentity

instance Semigroup IdentityMatcher where
  AnyIdentity <> x = x
  x <> AnyIdentity = x
  IdentityMatchAll xs <> IdentityMatchAll ys = IdentityMatchAll $ xs <> ys
  IdentityMatchAll xs <> y = IdentityMatchAll $ xs <> [y]
  x <> IdentityMatchAll ys = IdentityMatchAll $ x : ys
  x <> y = IdentityMatchAll [x, y]

identityMatches :: MonadGame env m => IdentityMatcher -> IdentityId -> m Bool
identityMatches matcher ident = member ident <$> gameSelectIdentity matcher

pattern IdentityWithAnyDamage :: IdentityMatcher
pattern IdentityWithAnyDamage <-
  IdentityWithDamage (GreaterThan (Static 0)) where
  IdentityWithAnyDamage = IdentityWithDamage (GreaterThan (Static 0))

data AllyMatcher
  = UnexhaustedAlly
  | ExhaustedAlly
  | AllyWithUses GameValueMatcher
  | AllyControlledBy IdentityMatcher
  | AllyWithDamage GameValueMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

pattern AllyWithAnyUses :: AllyMatcher
pattern AllyWithAnyUses <- AllyWithUses (GreaterThan (Static 0)) where
  AllyWithAnyUses = AllyWithUses (GreaterThan (Static 0))


data SupportMatcher = UnexhaustedSupport | SupportControlledBy IdentityMatcher

pattern UpgradeWithAnyUses :: UpgradeMatcher
pattern UpgradeWithAnyUses <- UpgradeWithUses (GreaterThan (Static 0)) where
  UpgradeWithAnyUses = UpgradeWithUses (GreaterThan (Static 0))

data UpgradeMatcher = UpgradeWithUses GameValueMatcher | UpgradeControlledBy IdentityMatcher | UnexhaustedUpgrade

data EnemyMatcher = AnyEnemy | EnemyWithId EnemyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

enemyMatches :: MonadGame env m => EnemyMatcher -> EnemyId -> m Bool
enemyMatches matcher ident = member ident <$> gameSelectEnemy matcher

data VillainMatcher
  = ActiveVillain
  | VillainWithId VillainId
  | VillainWithDamage GameValueMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TreacheryMatcher = AnyTreachery
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

treacheryMatches
  :: MonadGame env m => TreacheryMatcher -> TreacheryId -> m Bool
treacheryMatches matcher ident = member ident <$> gameSelectTreachery matcher

data SchemeMatcher = AnyScheme | MainScheme
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MinionMatcher
  = AnyMinion
  | MinionWithId MinionId
  | MinionWithDamage GameValueMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

minionMatches :: MonadGame env m => MinionMatcher -> MinionId -> m Bool
minionMatches matcher ident = member ident <$> gameSelectMinion matcher

data GameValueMatcher = AnyValue | GreaterThan GameValue
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

gameValueMatches :: MonadGame env m => GameValueMatcher -> Natural -> m Bool
gameValueMatches matcher n = case matcher of
  AnyValue -> pure True
  GreaterThan v -> do
    value <- fromIntegral <$> fromGameValue v
    pure $ n > value

pattern CharacterWithAnyDamage :: CharacterMatcher
pattern CharacterWithAnyDamage <-
  CharacterWithDamage (GreaterThan (Static 0)) where
  CharacterWithAnyDamage = CharacterWithDamage (GreaterThan (Static 0))

newtype CharacterMatcher = CharacterWithDamage GameValueMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
