module Marvel.Modifier.Types where

import Marvel.Prelude

import Marvel.Card.Def (CardMatcher)
import Marvel.Keyword
import Marvel.Trait.Types

data Modifier
  = ResourceCostReduction Natural
  | ThwartModifier Natural
  | AttackModifier Natural
  | SchemeModifier Natural
  | DefenseModifier Natural
  | AllyLimitModifier Natural
  | HitPointModifier Natural
  | HandSizeModifier Natural
  | TraitModifier Trait
  | KeywordModifier Keyword
  | LastSpecial
  | -- | the cards matching cost this many more resources to play
    AdditionalResourceCost CardMatcher Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
