module Marvel.Matchers where

import Marvel.Prelude

data IdentityMatcher
  = HeroIdentity
  | UnexhaustedIdentity
  | AnyIdentity
  | IdentityMatchAll [IdentityMatcher]

instance Monoid IdentityMatcher where
  mempty = AnyIdentity

instance Semigroup IdentityMatcher where
  AnyIdentity <> x = x
  x <> AnyIdentity = x
  IdentityMatchAll xs <> IdentityMatchAll ys = IdentityMatchAll $ xs <> ys
  IdentityMatchAll xs <> y = IdentityMatchAll $ xs <> [y]
  x <> IdentityMatchAll ys = IdentityMatchAll $ x : ys
  x <> y = IdentityMatchAll [x, y]

data AllyMatcher = UnexhaustedAlly
data EnemyMatcher = AnyEnemy
data VillainMatcher = ActiveVillain
data SchemeMatcher = AnyScheme | MainScheme
