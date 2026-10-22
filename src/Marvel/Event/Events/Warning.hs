module Marvel.Event.Events.Warning (
  warning,
  Warning (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

warning :: EventCard Warning
warning = event Warning Cards.warning

newtype Warning = Warning (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When a hero would take any amount of damage, reduce that amount by 1.
instance RunMessage Warning where
  runMessage msg e@(Warning attrs) = case msg of
    EventMessage ident (PlayedEvent _ _ (Just (W.IdentityTakeDamage hero _))) | ident == eventId attrs -> do
      preventDamage hero 1
      pure e
    _ -> Warning <$> runMessage msg attrs
