module Marvel.SideScheme.SideSchemes.PowerfulEnchantments (powerfulEnchantments, PowerfulEnchantments (..)) where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

powerfulEnchantments :: SideSchemeCard PowerfulEnchantments
powerfulEnchantments = sideScheme PowerfulEnchantments Cards.powerfulEnchantments (PerPlayer 2)

newtype PowerfulEnchantments = PowerfulEnchantments (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance HasModifiersFor PowerfulEnchantments where
  getModifiersFor _ (AttachmentRef aid) _ = do
    friendly <- attachmentHasFriendlyHost aid
    pure [CannotBeDiscarded | friendly]
  getModifiersFor _ _ _ = pure []

instance RunMessage PowerfulEnchantments where
  runMessage msg (PowerfulEnchantments attrs) = runQueueT $ case msg of
    _ -> PowerfulEnchantments <$> runMessage msg attrs
