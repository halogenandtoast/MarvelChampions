module Marvel.Message.Minion where

import Marvel.Prelude

import Marvel.Card.PlayerCard.Types (PlayerCard)
import Marvel.Card.EncounterCard
import Marvel.Damage
import Marvel.Id
import Marvel.Keyword (Keyword)
import Marvel.Ref

data MinionMessage
  = RevealMinion IdentityId
  | -- | defeated if its damage is as much as its hit points: as an ally treated as a
    -- minion comes to its printed hit points (Beguiled; RR Hit Points)
    MinionCheckHitPoints
  | MinionDefendedBy CharacterId
  | MinionDamaged Source Damage
  | MinionStunned Source
  | MinionConfused Source
  | MinionBecomeTough
  | MinionDefeated
  | -- | Defeat interrupts finished: clean up only if leaving play was not replaced.
    MinionLeavesPlay
  | -- | the damage defeats it, with this much past its hit points: past the window in
    -- which the excess of a hero's attack may be increased
    MinionDefeatedWithExcess Source Damage Natural
  | MinionHealed Natural
  | MinionHealAllDamage
  | MinionAttacksWithChange IdentityId Int [Keyword]
  | MinionAttacks IdentityId
  | -- | its attack by the character resolved: retaliate answers it
    MinionRetaliates CharacterId
  | MinionSchemes
  | MinionBeginScheme
  | MinionSchemed
  | MinionBeginAttack IdentityId
  | MinionEngagedIdentity IdentityId
  | MinionAttacked
  | MinionEndAttack
  | -- | its attack under way gains the keyword: overkill, piercing or ranged
    MinionAttackGains Keyword
  | -- | the minion gets +X (or -X) ATK for the attack under way
    MinionAttackChanged Int
  | -- | all damage from its attack under way is prevented
    MinionAttackDamagePrevented
  | AttachedUpgradeToMinion UpgradeId
  | AttachedToMinion AttachmentId
  | -- | it attacks the player's ally, the player being attacked
    MinionAttacksAlly IdentityId AllyId
  | -- | it attacks another enemy (Moondragon): its ATK as damage, no boost card, the
    -- enemy's retaliate answering it
    MinionAttacksEnemy EnemyId
  | -- | Resolve damage after interrupts to an enemy attacking another enemy.
    MinionAttackEnemyAfterInterrupt EnemyId
  | MinionDealtBoost EncounterCard
  | -- | its boost cards are turned faceup in turn, then any it is given meanwhile
    MinionFlipBoostCards
  | -- | a boost card's icons are added to its activation
    MinionAddBoost Natural
  | MinionAttackBecomesDividedEvenly
  | MinionHoldsCard PlayerCard
  | MinionAttackDamageRedirected EnemyId
  | MinionSchemesAgainst IdentityId
  | MinionSchemeAgainstEnded
  | MinionAttackBecomesIndirect
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
