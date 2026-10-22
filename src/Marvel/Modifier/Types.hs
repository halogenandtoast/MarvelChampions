module Marvel.Modifier.Types where

import Marvel.Prelude

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
  | -- | on a source: each instance of damage it deals is increased
    DamageDealtModifier Natural
  | -- | on a source: each instance of threat it removes is increased
    ThreatRemovedModifier Natural
  | RecoveryModifier Natural
  | CannotTakeDamage
  | -- | to the target threat of a scheme
    TargetThreatModifier Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
