module Marvel.Message.Minion where

import Marvel.Prelude

import Marvel.Damage
import Marvel.Id
import Marvel.Ref

data MinionMessage
  = RevealMinion IdentityId
  | MinionDefendedBy CharacterId
  | MinionDamaged Source Damage
  | MinionStunned Source
  | MinionConfused Source
  | MinionBecomeTough
  | MinionDefeated
  | MinionHealed Natural
  | MinionHealAllDamage
  | MinionAttacks IdentityId
  | MinionSchemes
  | MinionSchemed
  | MinionBeginAttack IdentityId
  | MinionEngagedIdentity IdentityId
  | MinionAttacked
  | AttachedUpgradeToMinion UpgradeId
  | AttachedToMinion AttachmentId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
