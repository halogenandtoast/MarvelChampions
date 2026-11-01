module Marvel.Event.Events.ShakeItOff (
  shakeItOff,
  ShakeItOff (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Status ()
import Marvel.Window qualified as W

shakeItOff :: EventCard ShakeItOff
shakeItOff = event ShakeItOff Cards.shakeItOff

newtype ShakeItOff = ShakeItOff (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | After a Guardian character takes damage from an attack (the window is only opened
-- for some damage), give that character a tough status card.
instance RunMessage ShakeItOff where
  runMessage msg e@(ShakeItOff attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent _ _ (Just (W.EnemyAttacksAndDamages _ character _))) | ident == eventId attrs -> do
      giveStatus character ToughStatus
      pure e
    _ -> ShakeItOff <$> runMessage msg attrs
