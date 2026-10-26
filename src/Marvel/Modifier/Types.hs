module Marvel.Modifier.Types where

import Marvel.Prelude

import {-# SOURCE #-} Marvel.Card.Def (CardMatcher)
import Marvel.Cost.Types (Cost)
import Marvel.Id (MainSchemeId)
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
  | -- | "your hand size is reduced by N"
    HandSizeReduction Natural
  | TraitModifier Trait
  | KeywordModifier Keyword
  | LastSpecial
  | -- | on a main scheme: an acceleration token that would be placed on it is placed on
    -- the one given instead
    RedirectAccelerationTokens MainSchemeId
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
  | -- | a scheme threat cannot be removed from by thwarting (Held Hostage)
    ThreatCannotBeRemovedByThwarting
  | -- | an identity that stays exhausted when it would ready
    CannotReady
  | -- | an identity that stays in the form it is in
    CannotChangeForm
  | -- | "your hero gets -1 THW": taken off, not below 0
    ThwartPenalty Natural
  | AttackPenalty Natural
  | DefensePenalty Natural
  | HandSizePenalty Natural
  | -- | an enemy not given a boost card as it activates
    NoActivationBoostCard

  | -- | on a scheme: a character thwarting it may use their ATK instead of their THW
    ThwartWithAttack
  | -- | on a side scheme: it cannot leave play, by defeat, discard or removal
    CannotLeavePlay

  | -- | an enemy the players cannot attack (Distracting Taunts)
    CannotBeAttacked
  | -- | an enemy whose attacks the player must defend with an ally they control, if
    -- able (Ramming Speed)
    DefendedByAllyIfAble
  | -- | on an identity: each "When Revealed" ability its player reveals resolves once
    -- more for each of these
    ResolveWhenRevealedAgain
  | -- | on an identity: it cannot attack the characters with this title
    CannotAttackTitled Text
  | -- | on an identity: its player cannot play the cards matching (Depowered)
    CannotPlayCards CardMatcher
  | -- | on a card: its "Alter-Ego Action" abilities cannot be triggered (Corrupted
    -- Timestream)
    CannotTriggerAlterEgoActions
  | -- | on a card: its printed text box is treated as blank (Tech Theft)
    Blank
  | -- | on an ally: it does not count against its controller's ally limit (Stinger)
    NotCountedAgainstAllyLimit
  | -- | on a hero: the damage of its basic attack may be divided among enemies (Giant
    -- Wasp), each one dealt damage attacked
    DividesBasicAttack
  | -- | on a hero: the threat its basic thwart removes may be divided among schemes,
    -- each one thwarted
    DividesBasicThwart
  | -- | on an identity: what its hero must also pay to make a basic attack (Mother's
    -- Orders)
    BasicAttackAdditionalCost Cost
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
