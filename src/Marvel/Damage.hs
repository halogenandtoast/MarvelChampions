module Marvel.Damage where

import Marvel.Prelude

import Marvel.Id

data Damage = Damage
  { damageAmount :: Natural
  , damageOverkill :: Bool
  , damageSource :: DamageSource
  , damageIgnoresTough :: Bool
  -- ^ dealt whether or not the character has a tough status card, which it keeps
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
toDamage n source = Damage {damageAmount = n, damageOverkill = False, damageSource = source, damageIgnoresTough = False}

withOverkill :: Damage -> Damage
withOverkill d = d { damageOverkill = True }

ignoringTough :: Damage -> Damage
ignoringTough d = d { damageIgnoresTough = True }
