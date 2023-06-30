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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable, ToJSONKey, FromJSONKey)

class IsRef a where
  toRef :: a -> Ref focus

instance IsRef VillainId where
  toRef = VillainRef

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
