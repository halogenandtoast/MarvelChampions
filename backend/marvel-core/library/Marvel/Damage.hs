module Marvel.Damage where

import Marvel.Prelude

import Marvel.Id

data Damage = Damage
  { damageAmount :: Natural
  , damageOverkill :: Bool
  , damageSource :: DamageSource
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data DamageSource
  = FromPlayerAttack IdentityId
  | FromAllyAttack AllyId
  | FromAttack
  | FromAbility
  | FromRetaliate
  | FromTreachery
  | FromConsequential
  | FromOverkill
  | FromAnyDamageSource
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

toDamage :: Natural -> DamageSource -> Damage
toDamage n = Damage n False

withOverkill :: Damage -> Damage
withOverkill d = d {damageOverkill = True}
