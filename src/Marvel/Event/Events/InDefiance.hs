module Marvel.Event.Events.InDefiance (
  inDefiance,
  InDefiance (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

inDefiance :: EventCard InDefiance
inDefiance = event InDefiance Cards.inDefiance

newtype InDefiance = InDefiance (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Interrupt: when an identity (any player's) would take damage from an attack,
-- prevent 2 of it. Draw 1 card.
instance RunMessage InDefiance where
  runMessage msg e@(InDefiance attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ (Just (W.IdentityTakeDamage target _))) | ident == eventId attrs -> do
      preventDamage target 2
      drawCards you 1
      pure e
    _ -> InDefiance <$> runMessage msg attrs
