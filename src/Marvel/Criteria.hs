module Marvel.Criteria where

import Marvel.Prelude

import Marvel.Ability.Types (AbilitySubType (..), AbilityType (..))
import {-# SOURCE #-} Marvel.Card.Def (CardMatcher)
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
  | UpgradeExists UpgradeMatcher
  | -- | a resource ability that pays only for a card matching, as "for an event"
    PayingFor CardMatcher
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

-- | The cards a resource ability may pay for, if it is limited to some.
payingForMatchers :: Criteria -> [CardMatcher]
payingForMatchers = \case
  PayingFor m -> [m]
  Criteria cs -> concatMap payingForMatchers cs
  _ -> []

-- | The form a "Hero …" or "Alter-Ego …" ability is used in.
abilityTypeCriteria :: AbilityType -> Criteria
abilityTypeCriteria = \case
  HeroInterrupt -> InHeroForm
  HeroResource -> InHeroForm
  HeroResponse -> InHeroForm
  HeroAction -> InHeroForm
  AlterEgoAction -> InAlterEgoForm
  AlterEgoResource -> InAlterEgoForm
  _ -> NoCriteria

-- | What a thwart needs of the player making it.
abilitySubTypeCriteria :: AbilitySubType -> Criteria
abilitySubTypeCriteria = \case
  Thwart -> SelfMatches IdentityCanThwart
  _ -> NoCriteria
