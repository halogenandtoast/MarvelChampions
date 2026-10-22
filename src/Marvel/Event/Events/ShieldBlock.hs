module Marvel.Event.Events.ShieldBlock (
  shieldBlock,
  ShieldBlock (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards

shieldBlock :: EventCard ShieldBlock
shieldBlock = event ShieldBlock Cards.shieldBlock

newtype ShieldBlock = ShieldBlock (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The shield is exhausted as its cost, and all of the damage the player would take is
-- prevented.
instance RunMessage ShieldBlock where
  runMessage msg e@(ShieldBlock attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      cancelDamage identityId
      pure e
    _ -> ShieldBlock <$> runMessage msg attrs
