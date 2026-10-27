{-# LANGUAGE TypeData #-}

module Marvel.Ref where

import Marvel.Prelude

import Marvel.Card.Id
import Marvel.Id

type data Focus = Target' | Source'

data Ref (focus :: Focus)
  = IdentityRef IdentityId
  | VillainRef VillainId
  | MinionRef MinionId
  | EnemyRef EnemyId
  | AllyRef AllyId
  | CharacterRef CharacterId
  | SupportRef SupportId
  | EventRef EventId
  | EffectRef EffectId
  | TreacheryRef TreacheryId
  | ObligationRef ObligationId
  | UpgradeRef UpgradeId
  | SideSchemeRef SideSchemeId
  | AttachmentRef AttachmentId
  | MainSchemeRef MainSchemeId
  | SchemeRef SchemeId
  | CardIdRef CardId
  | ActiveCostRef ActiveCostId
  | GameRef
  | EnvironmentRef EnvironmentId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable, ToJSONKey, FromJSONKey)

class IsRef a where
  toRef :: a -> Ref focus

instance IsRef VillainId where
  toRef = VillainRef

instance IsRef TreacheryId where
  toRef = TreacheryRef

instance IsRef MinionId where
  toRef = MinionRef

instance IsRef IdentityId where
  toRef = IdentityRef

instance IsRef SideSchemeId where
  toRef = SideSchemeRef

instance IsRef SupportId where
  toRef = SupportRef

instance IsRef MainSchemeId where
  toRef = MainSchemeRef

instance IsRef UpgradeId where
  toRef = UpgradeRef

instance IsRef CharacterId where
  toRef = CharacterRef

instance IsRef SchemeId where
  toRef = SchemeRef

instance IsRef EnemyId where
  toRef = EnemyRef

instance IsRef AttachmentId where
  toRef = AttachmentRef

instance IsRef EnvironmentId where
  toRef = EnvironmentRef

instance IsRef AllyId where
  toRef = AllyRef

instance IsRef (Ref focus) where
  toRef = refocus

instance (IsRef a) => IsRef (With a b) where
  toRef = toRef . unWith

refocus :: Ref focus -> Ref focus'
refocus = coerce

type Source = Ref Source'

toSource :: (IsRef a) => a -> Source
toSource = toRef

type Target = Ref Target'

toTarget :: (IsRef a) => a -> Target
toTarget = toRef

-- | An enemy, by any id that names one.
class IsEnemy a where
  toEnemy :: a -> EnemyId

instance IsEnemy EnemyId where
  toEnemy = id

instance IsEnemy VillainId where
  toEnemy = EnemyVillainId

instance IsEnemy MinionId where
  toEnemy = EnemyMinionId

-- | The enemy's own ref, a villain's or a minion's.
enemyRef :: (IsEnemy a) => a -> Target
enemyRef a = case toEnemy a of
  EnemyVillainId v -> VillainRef v
  EnemyMinionId m -> MinionRef m

-- | A character, by any id that names one.
class IsCharacter a where
  toCharacter :: a -> CharacterId

instance IsCharacter CharacterId where
  toCharacter = id

instance IsCharacter IdentityId where
  toCharacter = IdentityCharacter

instance IsCharacter AllyId where
  toCharacter = AllyCharacter

instance IsCharacter VillainId where
  toCharacter = VillainCharacter

instance IsCharacter MinionId where
  toCharacter = MinionCharacter

instance IsCharacter EnemyId where
  toCharacter = \case
    EnemyVillainId v -> VillainCharacter v
    EnemyMinionId m -> MinionCharacter m

-- | A scheme, by any id that names one.
class IsScheme a where
  toScheme :: a -> SchemeId

instance IsScheme SchemeId where
  toScheme = id

instance IsScheme MainSchemeId where
  toScheme = SchemeMainSchemeId

instance IsScheme SideSchemeId where
  toScheme = SchemeSideSchemeId

-- | The scheme's own ref, a main scheme's or a side scheme's.
schemeRef :: (IsScheme a) => a -> Target
schemeRef a = case toScheme a of
  SchemeMainSchemeId m -> MainSchemeRef m
  SchemeSideSchemeId s -> SideSchemeRef s
