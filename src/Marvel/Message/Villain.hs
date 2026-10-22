module Marvel.Message.Villain where

import Marvel.Prelude

import Marvel.Card.EncounterCard
import Marvel.Damage
import Marvel.Id
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
  | VillainAttackGainOverkill
  | VillainAttacks IdentityId
  | VillainBeginAttack IdentityId
  | VillainEndAttack
  | VillainAttacked
  | VillainSchemes
  | VillainSchemed
  | VillainHealed Natural
  | VillainDefeated
  | VillainDefendedBy CharacterId
  | AttachedToVillain AttachmentId
  | AttachedUpgradeToVillain UpgradeId
  | VillainAdvanced
  | -- | its hit points (and remaining hit points) go up or down by this much
    VillainHitPointsChanged Int
  | -- | a boost card's icons are added to the activation
    VillainAddBoost Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
