module Marvel.Message.Ally where

import Marvel.Prelude

import Marvel.Attack
import Marvel.Damage
import Marvel.Id
import Marvel.Payment.Types
import Marvel.Ref

data AllyMessage
  = ExhaustedAlly
  | ReadiedAlly
  | AllyAttacked
  | -- | the interrupts to the attack resolved: the damage lands, if the enemy is still
    -- in play to take it
    AllyAttackLands EnemyId Damage
  | AllyThwarted
  | -- | the thwart done: its consequential damage, as the ally is then
    AllyThwartConsequential
  | AllyWasAttacked Attack
  | -- | retaliate's damage to the enemy that attacked it
    AllyRetaliates EnemyId Natural
  | AllyDamaged Source Damage
  | AllyDefended EnemyId
  | AllyDefeated
  | -- | defeated by damage from the enemy's attack
    AllyDefeatedByAttack EnemyId
  | AllyHealed Natural
  | SpendAllyUse
  | AttachedUpgradeToAlly UpgradeId
  | AllyStunned
  | AllyPaidWith Payment
  | -- | whose deck it came from, and whose discard pile it goes to
    AllyOwnedBy IdentityId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
