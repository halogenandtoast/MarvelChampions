module Marvel.Modifier.Types where

import Marvel.Prelude

import {-# SOURCE #-} Marvel.Card.Def (CardMatcher)
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
  | -- | the cost of the cards matching is reduced
    ResourceCostReductionFor CardMatcher Natural
  | -- | the player cannot thwart: no basic thwart, and no thwart ability, for any card
    -- they control
    CannotThwart
  | -- | more consequential damage after the ally attacks
    AttackConsequentialDamageModifier Natural
  | -- | on a source: each instance of damage it deals is increased
    DamageDealtModifier Natural
  | -- | on a source: each instance of threat it removes is increased
    ThreatRemovedModifier Natural
  | RecoveryModifier Natural
  | CannotTakeDamage
  | -- | to the target threat of a scheme
    TargetThreatModifier Natural
  | -- | the cards matching cost this many more resources to play
    AdditionalResourceCost CardMatcher Natural
  | -- | on a character: the cost to play each upgrade on it is reduced
    UpgradeOnThisCostReduction Natural
  | -- | a scheme threat cannot be removed from
    ThreatCannotBeRemoved
  | -- | an identity that stays exhausted when it would ready
    CannotReady
  | -- | an identity that stays in the form it is in
    CannotChangeForm
  | -- | an enemy not given a boost card as it activates
    NoActivationBoostCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
