module Marvel.Damage where

import Marvel.Prelude

import Marvel.Id

data Damage = Damage
  { damageAmount :: Natural
  , damageOverkill :: Bool
  , damageSource :: DamageSource
  , damageIgnoresTough :: Bool
  -- ^ dealt whether or not the character has a tough status card, which it keeps
  , damagePiercing :: Bool
  -- ^ the character's tough status card is discarded before it is dealt (RR, Piercing)
  , damageRanged :: Bool
  -- ^ the attack it is ignores retaliate (RR, Ranged)
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
toDamage n source =
  Damage
    { damageAmount = n
    , damageOverkill = False
    , damageSource = source
    , damageIgnoresTough = False
    , damagePiercing = False
    , damageRanged = False
    }

withOverkill :: Damage -> Damage
withOverkill d = d { damageOverkill = True }

ignoringTough :: Damage -> Damage
ignoringTough d = d { damageIgnoresTough = True }

-- | "This attack gains piercing."
withPiercing :: Damage -> Damage
withPiercing d = d { damagePiercing = True }

-- | "This attack gains ranged."
withRanged :: Damage -> Damage
withRanged d = d { damageRanged = True }

-- | Whether a tough status card on the character prevents the damage, as it does
-- unless the damage ignores it or pierces it.
preventedByTough :: Damage -> Bool
preventedByTough d = not (damageIgnoresTough d || damagePiercing d)

-- | Whether the damage discards the character's tough status card as it is dealt: a
-- piercing attack's, if it deals any.
piercesTough :: Damage -> Bool
piercesTough d = damagePiercing d && damageAmount d > 0
