module Marvel.Message.Minion where

import Marvel.Prelude

import Marvel.Card.EncounterCard
import Marvel.Damage
import Marvel.Id
import Marvel.Keyword (Keyword)
import Marvel.Ref

data MinionMessage
  = RevealMinion IdentityId
  | MinionDefendedBy CharacterId
  | MinionDamaged Source Damage
  | MinionStunned Source
  | MinionConfused Source
  | MinionBecomeTough
  | MinionDefeated
  | -- | the damage defeats it, with this much past its hit points: past the window in
    -- which the excess of a hero's attack may be increased
    MinionDefeatedWithExcess Source Damage Natural
  | MinionHealed Natural
  | MinionHealAllDamage
  | MinionAttacks IdentityId
  | -- | its attack by the character resolved: retaliate answers it
    MinionRetaliates CharacterId
  | MinionSchemes
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
  | MinionDealtBoost EncounterCard
  | -- | its boost cards are turned faceup in turn, then any it is given meanwhile
    MinionFlipBoostCards
  | -- | a boost card's icons are added to its activation
    MinionAddBoost Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
