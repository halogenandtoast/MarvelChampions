module Marvel.Damage where

import Marvel.Prelude
import Data.Aeson.KeyMap qualified as KeyMap

import Marvel.Id

data Damage = Damage
  { damageAmount :: Natural
  , damagePrevention :: Natural
  -- ^ prevention applied after incoming enemy damage modifiers and caps
  , damageIndirect :: Bool
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
  deriving anyclass (ToJSON, Hashable)

instance FromJSON Damage where
  parseJSON = withObject "Damage" $ \o -> genericParseJSON defaultOptions $ Object $
    KeyMap.insertWith (\_ existing -> existing) "damagePrevention" (toJSON (0 :: Natural)) $
    KeyMap.insertWith (\_ existing -> existing) "damageIndirect" (Bool False) o

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
    , damagePrevention = 0
    , damageIndirect = False
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
-- unless the damage ignores it or pierces it. No damage is none to prevent: the card
-- stays (RR Tough: "would take damage").
preventedByTough :: Damage -> Bool
preventedByTough d = damageAmount d > 0 && not (damageIgnoresTough d || damagePiercing d)

-- | Whether the damage discards the character's tough status card as it is dealt: a
-- piercing attack's, if it deals any.
piercesTough :: Damage -> Bool
piercesTough d = damagePiercing d && damageAmount d > 0
