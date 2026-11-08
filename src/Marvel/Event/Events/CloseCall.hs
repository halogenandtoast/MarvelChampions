module Marvel.Event.Events.CloseCall (
  closeCall,
  CloseCall (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

closeCall :: EventCard CloseCall
closeCall = event CloseCall Cards.closeCall

newtype CloseCall = CloseCall (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Interrupt: when a boost card is turned faceup (any enemy's, attack or scheme),
-- cancel its "Boost" ability and all of its boost icons, then discard it. Draw 1 card.
-- The pending boost resolution and that card's icon count are taken off the queue.
instance RunMessage CloseCall where
  runMessage msg e@(CloseCall attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ (Just (W.FlippedBoostCard card _))) | ident == eventId attrs -> do
      discardBoostCardBeforeReveal card
      drawCards you 1
      pure e
    _ -> CloseCall <$> runMessage msg attrs
