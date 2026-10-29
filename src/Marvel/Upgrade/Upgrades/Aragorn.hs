module Marvel.Upgrade.Upgrades.Aragorn (aragorn, Aragorn (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Trait (Trait (Aerial))
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

aragorn :: UpgradeCard Aragorn
aragorn = upgrade Aragorn Cards.aragorn

newtype Aragorn = Aragorn (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- RR errata: "You", so both the hero and the alter-ego receive these modifiers.
instance HasModifiersFor Aragorn where
  getModifiersFor _ (IdentityRef ident) (Aragorn attrs) | ident == upgradeController attrs =
    pure [HitPointModifier 4, TraitModifier Aerial]
  getModifiersFor _ _ _ = pure []

instance RunMessage Aragorn where
  runMessage msg (Aragorn attrs) = runQueueT $ case msg of
    _ -> Aragorn <$> runMessage msg attrs
