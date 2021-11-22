{-# LANGUAGE PatternSynonyms #-}
module Marvel.Matchers where

import Marvel.Prelude

import {-# SOURCE #-} Marvel.Card.Def
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id

data EntityMatcher = IdentityEntity IdentityMatcher | AllyEntity AllyMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

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
  deriving anyclass (ToJSON, FromJSON, Hashable)

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
  | AllyWithId AllyId
  | AnyAlly
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

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
  deriving anyclass (ToJSON, FromJSON, Hashable)

enemyMatches :: MonadGame env m => EnemyMatcher -> EnemyId -> m Bool
enemyMatches matcher ident = member ident <$> gameSelectEnemy matcher

data VillainMatcher
  = ActiveVillain
  | VillainWithId VillainId
  | VillainWithDamage GameValueMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

villainMatches :: MonadGame env m => VillainMatcher -> VillainId -> m Bool
villainMatches matcher ident = member ident <$> gameSelectVillain matcher

data TreacheryMatcher = AnyTreachery
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

treacheryMatches
  :: MonadGame env m => TreacheryMatcher -> TreacheryId -> m Bool
treacheryMatches matcher ident = member ident <$> gameSelectTreachery matcher

data SchemeMatcher = AnyScheme | MainScheme
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data MinionMatcher
  = AnyMinion
  | MinionWithId MinionId
  | MinionWithDamage GameValueMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

minionMatches :: MonadGame env m => MinionMatcher -> MinionId -> m Bool
minionMatches matcher ident = member ident <$> gameSelectMinion matcher

data GameValueMatcher = AnyValue | GreaterThan GameValue
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

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
  deriving anyclass (ToJSON, FromJSON, Hashable)

data ExtendedCardMatcher
  = AffordableCardBy IdentityMatcher
  | BasicCardMatches CardMatcher
  | InDiscardOf IdentityMatcher ExtendedCardMatcher
  | ExtendedCardMatches [ExtendedCardMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup ExtendedCardMatcher where
  ExtendedCardMatches xs <> ExtendedCardMatches ys =
    ExtendedCardMatches $ xs <> ys
  x <> ExtendedCardMatches ys = ExtendedCardMatches $ x : ys
  ExtendedCardMatches xs <> y = ExtendedCardMatches $ xs <> [y]
  x <> y = ExtendedCardMatches [x, y]
