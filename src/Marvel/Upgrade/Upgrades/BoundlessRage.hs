module Marvel.Upgrade.Upgrades.BoundlessRage (
  boundlessRage,
  BoundlessRage (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

boundlessRage :: UpgradeCard BoundlessRage
boundlessRage = upgrade BoundlessRage Cards.boundlessRage

newtype BoundlessRage = BoundlessRage (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Hulk gets +1 ATK."
instance HasModifiersFor BoundlessRage where
  getModifiersFor _ (IdentityRef ident) (BoundlessRage a)
    | ident == upgradeController a = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

-- | "Forced Response: After you change form, discard this card."
instance HasAbilities BoundlessRage where
  getAbilities (BoundlessRage a) =
    [ limitedWindowAbility
        a
        1
        (IdentityChangedToForm After $ IdentityWithId $ upgradeController a)
        ForcedResponse
        OwnsThis
        NoCost
        (DiscardTarget $ toTarget a)
    ]

instance RunMessage BoundlessRage where
  runMessage msg (BoundlessRage attrs) = BoundlessRage <$> runMessage msg attrs
