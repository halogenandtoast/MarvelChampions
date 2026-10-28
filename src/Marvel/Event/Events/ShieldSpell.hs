module Marvel.Event.Events.ShieldSpell (
  shieldSpell,
  ShieldSpell (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

shieldSpell :: EventCard ShieldSpell
shieldSpell = event ShieldSpell Cards.shieldSpell

newtype ShieldSpell = ShieldSpell (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Interrupt: When you would take any amount of damage from an attack, discard
-- that many cards from the top of your deck (its cost, 'DiscardTopOfDeckForDamageCost')
-- → prevent all damage from this attack.
instance RunMessage ShieldSpell where
  runMessage msg e@(ShieldSpell attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      cancelDamage you
      pure e
    _ -> ShieldSpell <$> runMessage msg attrs
