module Marvel.Message.Ally where

import Marvel.Prelude

import Marvel.Attack
import Marvel.Damage
import Marvel.Id
import Marvel.Source

data AllyMessage
  = ExhaustedAlly
  | ReadiedAlly
  | AllyAttacked
  | AllyThwarted
  | AllyWasAttacked Attack
  | AllyDamaged Source Damage
  | AllyDefended EnemyId
  | AllyDefeated
  | AllyHealed Natural
  | SpendAllyUse
  | AttachedUpgradeToAlly UpgradeId
  | AllyStunned
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
