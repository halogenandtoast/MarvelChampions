module Marvel.Matchers where

import Marvel.Prelude

import Marvel.Game.Source
import Marvel.Id

data IdentityMatcher
  = HeroIdentity
  | AlterEgoIdentity
  | UnexhaustedIdentity
  | AnyIdentity
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

data AllyMatcher = UnexhaustedAlly | ExhaustedAlly
data SupportMatcher = UnexhaustedSupport
data EnemyMatcher = AnyEnemy
data VillainMatcher = ActiveVillain

data TreacheryMatcher = AnyTreachery
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

treacheryMatches
  :: MonadGame env m => TreacheryMatcher -> TreacheryId -> m Bool
treacheryMatches matcher ident = member ident <$> gameSelectTreachery matcher

data SchemeMatcher = AnyScheme | MainScheme

data MinionMatcher = AnyMinion
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data GameValueMatcher = AnyValue
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

gameValueMatches :: MonadGame env m => GameValueMatcher -> Natural -> m Bool
gameValueMatches matcher _ = case matcher of
  AnyValue -> pure True
