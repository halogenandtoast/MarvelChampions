module Marvel.Message.Ally where

import Marvel.Prelude

import Marvel.Card.EncounterCard (EncounterCard)
import Marvel.Attack
import Marvel.Damage
import Marvel.Id
import Marvel.Payment.Types
import Marvel.Ref

data AllyMessage
  = ExhaustedAlly
  | ReadiedAlly
  | -- | another player takes control of it, in their play area (Odin following the
    -- first player)
    AllyChangedController IdentityId
  | -- | the damage on it set, as it was a minion's (back from being one)
    AllySetDamage Natural
  | -- | it is flipped to its other face (Odin, Captive to King): the same card in play,
    -- its damage, status cards, counters, upgrades and attachments kept (RR Flip)
    FlipAlly
  | -- | readied by the game or an encounter card, not a player card (Unnatural Storm)
    ReadiedAllyByGame
  | AllyAttacked
  | -- | the interrupts to the attack resolved: the damage lands, if the enemy is still
    -- in play to take it
    AllyAttackLands EnemyId Damage
  | AllyAssignEvenAttack Damage Natural [(EnemyId, Natural)] [EnemyId] [EnemyId]
  | AllyEvenAttackLands Damage [(EnemyId, Natural)]
  | AllyEvenAttackFinished Damage [EnemyId]
  | AllyAttackMultiple [EnemyId] Damage
  | AllyAttackMultipleNext [EnemyId] [EnemyId] Damage
  | AllyAttackMultipleFinished [EnemyId]
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
  | AllyHoldsTreachery EncounterCard
  | AllyPaidWith Payment
  | -- | whose deck it came from, and whose discard pile it goes to
    AllyOwnedBy IdentityId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
