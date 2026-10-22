module Marvel.Criteria where

import Marvel.Prelude

import Marvel.Ability.Types (AbilityType (..))
import {-# SOURCE #-} Marvel.Card.Def (CardMatcher)
import Marvel.Matchers.Types
import Marvel.Resource.Types

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
  | -- | a resource ability that pays only for a card matching, as "for an event"
    PayingFor CardMatcher
  | -- | the card's resource cost is paid with only this type of resource: "you can
    -- only spend [physical] resources to pay for this card"
    SpendOnly Resource
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

-- | The type of resource a card's cost must be paid with, if only one may be.
spendOnly :: Criteria -> Maybe Resource
spendOnly = \case
  SpendOnly r -> Just r
  Criteria xs -> headMay (mapMaybe spendOnly xs)
  _ -> Nothing

-- | Whether a resource may be spent on a card with these criteria: a wild resource is
-- spent as any type.
canSpendOn :: Criteria -> Resource -> Bool
canSpendOn criteria r = maybe True (\only -> r == only || r == Wild) (spendOnly criteria)
