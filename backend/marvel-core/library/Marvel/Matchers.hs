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

data AllyMatcher = UnexhaustedAlly | ExhaustedAlly | AllyControlledBy IdentityMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SupportMatcher = UnexhaustedSupport | SupportControlledBy IdentityMatcher
data EnemyMatcher = AnyEnemy
data VillainMatcher = ActiveVillain

data TreacheryMatcher = AnyTreachery
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

treacheryMatches
  :: MonadGame env m => TreacheryMatcher -> TreacheryId -> m Bool
treacheryMatches matcher ident = member ident <$> gameSelectTreachery matcher

data SchemeMatcher = AnyScheme | MainScheme
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MinionMatcher = AnyMinion | MinionWithId MinionId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

minionMatches
  :: MonadGame env m => MinionMatcher -> MinionId -> m Bool
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
