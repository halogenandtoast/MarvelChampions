module Marvel.Message.Villain where

import Marvel.Prelude

import Marvel.Card.EncounterCard
import Marvel.Damage
import Marvel.Id
import Marvel.Keyword (Keyword)
import Marvel.Ref

data VillainMessage
  = SetVillainHp
  | VillainDamaged Source Damage
  | VillainStunned Source
  | VillainConfused Source
  | VillainBecomeTough
  | VillainDealtBoost EncounterCard
  | VillainFlipBoostCards
  | VillainCheckAdditionalBoosts
  | -- | its attack under way gains the keyword: overkill, piercing or ranged
    VillainAttackGains Keyword
  | -- | the villain gets +X (or -X) ATK for the attack under way
    VillainAttackChanged Int
  | -- | all damage from its attack under way is prevented
    VillainAttackDamagePrevented
  | VillainAttacks IdentityId
  | -- | it attacks the player, the attack given no boost card (Escaped Convict)
    VillainAttacksWithoutBoost IdentityId
  | -- | its attack by the character resolved: retaliate answers it
    VillainRetaliates CharacterId
  | VillainBeginAttack IdentityId
  | VillainBeginAttackWithoutBoost IdentityId
  | VillainEndAttack
  | VillainAttacked
  | VillainSchemes
  | -- | its scheme under way, past a confusion: what a replaced scheme never reaches
    VillainBeginScheme
  | VillainSchemed
  | VillainHealed Natural
  | VillainDefeated
  | VillainDefendedBy CharacterId
  | AttachedToVillain AttachmentId
  | AttachedUpgradeToVillain UpgradeId
  | VillainAdvanced
  | -- | it turns to its other face, keeping its damage, status cards, attachments and
    -- boosts: the villain answers it with @flipVillainTo@
    FlipVillain
  | -- | its hit points (and remaining hit points) go up or down by this much
    VillainHitPointsChanged Int
  | -- | a boost card's icons are added to the activation
    VillainAddBoost Natural
  | -- | its next activation gets this much more SCH or ATK ("+1 ATK for this
    -- activation")
    VillainActivationBonus Natural
  | -- | it attacks the player's ally, the player being attacked
    VillainAttacksAlly IdentityId AllyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
