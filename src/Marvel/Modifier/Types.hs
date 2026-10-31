module Marvel.Modifier.Types where

import Marvel.Prelude
import Marvel.Ref (Target)

import Marvel.Card.Id (CardId)
import Marvel.Id (EnemyId)

import {-# SOURCE #-} Marvel.Card.Def (CardMatcher)
import Marvel.Cost.Types (Cost)
import Marvel.Id (IdentityId, MainSchemeId)
import Marvel.Resource.Types (Resource)
import Marvel.Keyword
import Marvel.Trait.Types

data Modifier
  = TakesFirstPlayerPhaseTurn
  | AttackWithThwart
  | AttackConsequentialDamageReduction Natural
  | AttacksAgainstGainKeyword Keyword
  | AttackDamageTakenModifier Int
  | IdentityCountersAsResources
  | CanHaveAnyNumberOfToughStatusCards
  | AdditionalToughStatusCards Natural
  | CannotBeStunned
  | CannotBeConfused
  | LoseKeyword Keyword
  | CannotRecover
  | CannotBeHealedByPlayerCards
  | CannotHaveAttachedPlayerCards
  | ContinuouslyStunned
  | ContinuouslyConfused
  | CannotChangeMassForm
  | SchemesWithoutBoost
  | MaximumSustainedDamage Natural
  | MaximumDamagePerPhase Natural
  | NoAttackConsequentialDamage
  | NoThwartConsequentialDamage
  | CanLookAtTopOfEncounterDeck
  | PrintedResourcesAs Resource
  | DividesBasicAttackEvenly
  | ResourceCostReduction Natural
  | -- | on a resource ability source: its effect can generate identity counters
    MayGenerateIdentityCountersFor CardMatcher Natural
  | -- | reduction scoped to one physical card during one nested play
    ResourceCostReductionForCard CardId Natural
  | ThwartModifier Natural
  | AttackReductionWhileAttacking IdentityId Natural
  | AttackModifier Natural
  | SchemeModifier Natural
  | AttackModifierAgainst EnemyId Natural
  | DefenseModifierAgainst EnemyId Natural
  | AttacksCannotBeDefended
  | ReplaceHeroAttackWithDiscardAndStun Target
  | CannotAttackEnemy EnemyId
  | CannotBeDiscarded
  | DefenseModifier Natural
  | AllyLimitModifier Natural
  | -- | on an identity: it can control this many more restricted upgrades (Venom)
    RestrictedLimitModifier Natural
  | -- | on an identity: it can control this many more restricted upgrades, each a Weapon
    -- (Side Holster)
    RestrictedWeaponLimitModifier Natural
  | HitPointModifier Natural
  | HandSizeModifier Natural
  | -- | "your hand size is reduced by N"
    HandSizeReduction Natural
  | TraitModifier Trait
  | KeywordModifier Keyword
  | -- | on an identity: its attacks ignore guard, its thwarts patrol or crisis
    IgnoresGuard
  | IgnoresPatrol
  | IgnoresCrisis
  | -- | prevents all attacks/defenses, including labeled abilities (Intangible)
    CannotAttack
  | CannotActivate
  | CannotDefend
  | -- | reduce each instance of consequential damage, to a minimum of zero
    ConsequentialDamageReduction Natural
  | LastSpecial
  | -- | on a main scheme: an acceleration token that would be placed on it is placed on
    -- the one given instead
    RedirectAccelerationTokens MainSchemeId
  | -- | the cost of the cards matching is reduced
    ResourceCostReductionFor CardMatcher Natural
  | -- | the player cannot thwart: no basic thwart, and no thwart ability, for any card
    -- they control
    CannotThwart
  | CannotTriggerPrintedHeroAbilities
  | -- | more consequential damage after the ally attacks
    AttackConsequentialDamageModifier Natural
  | -- | on a source: each instance of damage it deals is increased
    DamageDealtModifier Natural
  | -- | Charge counters Gambit spent to empower this event.
    ChargedWithCounters Natural
  | -- | A continuous cost-setting ability is reapplied during resource payment.
    ContinuouslyRecalculateResourceCost
  | DamageTakenModifier Int
  | AdditionalActivationBoostIcons Natural
  | -- | on a source: each instance of threat it removes is increased
    ThreatRemovedModifier Natural
  | -- | a divided thwart counts its threat-removal bonuses once in the total pool
    ThreatRemovalBonusInTotal
  | RecoveryModifier Natural
  | CannotTakeDamage
  | -- | on a character: it takes no damage from a player card, or from an effect one
    -- created (Sanctuary, errata)
    CannotTakeDamageFromPlayerCards
  | -- | on a villain: each amount of damage it would take, from any source, is reduced,
    -- after a tough status card has prevented what it does (Thanos's Armor)
    DamageReduction Natural
  | -- | on a character: it has infinite hit points: damage is dealt to it, but it is not
    -- defeated by it (The Galaxy's Most Wanted, Escape the Museum)
    InfiniteHitPoints
  | -- | to the target threat of a scheme
    TargetThreatModifier Natural
  | -- | the cards matching cost this many more resources to play
    AdditionalResourceCost CardMatcher Natural
  | -- | on a player: the cards they play are played ignoring their resource cost, the
    -- additional costs on them included (RR "Ignore")
    IgnoresResourceCost
  | IgnoresResourceCostForCard CardId
  | PlayedUsingWolverinesClaws
  | BasicAttackKeyword Keyword
  | -- | on the game: each encounter card has this many more boost icons
    AdditionalBoostIcons Natural
  | -- | on a character: the cost to play each upgrade on it is reduced
    UpgradeOnThisCostReduction Natural
  | -- | on a side scheme: only this player's cards remove threat from it, and no one
    -- else may thwart it (Sibling Rivalry)
    ThreatRemovableOnlyBy IdentityId
  | -- | a scheme threat cannot be removed from
    ThreatCannotBeRemoved
  | -- | a scheme threat cannot be removed from by thwarting (Held Hostage)
    ThreatCannotBeRemovedByThwarting
  | -- | an identity that stays exhausted when it would ready
    CannotReady
  | -- | an identity that stays in the form it is in
    CannotChangeForm
  | CannotChangeToHeroForm
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
  | -- | on an enemy: the damage it takes from each attack is reduced
    AttackDamageReduction Natural
  | -- | on an enemy: it takes no more than this much damage from a single attack
    -- (Cutthroat Ambition), after any reduction
    AttackDamageCap Natural
  | CannotBeCanceled
  | AccelerationIconModifier Natural
  | HazardIconModifier Natural
  | CannotThwartSideSchemes
  | CrisisIcon
  | AdditionalThwartCost Cost
  | BasicThwartOnlyMostThreat
  | BasicThwartIgnoresPatrol
  | -- | on an identity: an interrupt it can use as it plays a card from hand reduces the
    -- card's cost by this much (Star-Lord), counted toward what it can afford
    MayReduceCostToPlay Natural
  | -- | on an ally: it cannot attack minions (Drax)
    CannotAttackMinions
  | -- | on an ally: it takes no consequential damage
    NoConsequentialDamage
  | -- | on an identity: what its hero must also pay to make a basic attack (Mother's
    -- Orders)
    BasicAttackAdditionalCost Cost
  | -- | on an attachment: it cannot be moved from the character it is attached to
    -- (Superior Tactics)
    CannotBeUnattached
  -- Tower Defense
  | -- | on the game: "the villain" of a card a player plays or an ability they trigger is
    -- the villain they choose (Focused Defense; MTS rulebook p.11)
    PlayersChooseTheVillain
  | -- | on the game: the unique rule does not apply to the cards with this title
    -- (Avengers Tower, Stronghold)
    UniqueRuleDoesNotApply Text
  | -- | on a villain: damage takes him to 0 hit points at most, but he is not defeated
    -- by it ("cannot be defeated while …", Proxima Midnight and Corvus Glaive)
    CannotBeDefeated
  -- Spectrum
  | -- | on an identity: its energy form does not change (Loss of Control); turning
    -- them all facedown is not changing it
    CannotChangeEnergyForm
  -- Hela (and Enchantress)
  | -- | on an ally: no card may be attached to it, no upgrade played on it (Odin, Captive)
    CannotHaveAttachedCards
  | -- | on an ally: no encounter card may be attached to it (Odin, King)
    CannotHaveAttachedEncounterCards
  | -- | on an ally: the first player controls it, and it follows the first player token
    -- (Odin)
    ControlledByFirstPlayer
  -- The Mad Titan's Shadow campaign
  | -- | on an ally: its basic thwart ignores any crisis icons in play (Fandral)
    BasicThwartIgnoresCrisis
  | -- | on a hero or an ally: a player card's ability does not ready it; the ready phase
    -- and encounter cards still do (Unnatural Storm)
    CannotBeReadiedByPlayerCards
  | -- | on an identity: it cannot make basic attacks, nor attempt one to remove a stun
    -- (Seduced)
    CannotBasicAttack
  | -- | on an ally: leaving play, it is removed from the game instead, never discarded or
    -- returned to hand (Odin, King)
    RemovedFromGameInsteadOfLeavingPlay
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
