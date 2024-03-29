module Marvel.Criteria where

import Marvel.Prelude

import Marvel.Matchers.Types

data Criteria
  = IsSelf
  | NoCriteria
  | InHeroForm
  | InAlterEgoForm
  | Unexhausted
  | Exhausted
  | OwnsThis
  | MinionExists MinionMatcher
  | EnemyExists EnemyMatcher
  | SchemeExists SchemeMatcher
  | AllyExists AllyMatcher
  | CharacterExists CharacterMatcher
  | SelfMatches IdentityMatcher
  | ExtendedCardExists ExtendedCardMatcher
  | Never
  | Criteria [Criteria]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Monoid Criteria where
  mempty = NoCriteria

instance Semigroup Criteria where
  NoCriteria <> x = x
  x <> NoCriteria = x
  Criteria xs <> Criteria ys = Criteria $ xs <> ys
  Criteria xs <> y = Criteria $ xs <> [y]
  x <> Criteria ys = Criteria $ x : ys
  x <> y = Criteria [x, y]
