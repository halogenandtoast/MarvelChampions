module Marvel.Event.Events.Meditation (meditation, Meditation (..)) where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source (canPlayCardWithReduction, getAvailablePaymentSources)
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

meditation :: EventCard Meditation
meditation = event Meditation Cards.meditation

newtype Meditation = Meditation (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Meditation where
  runMessage msg e@(Meditation attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      cards <- filterM (canPlayCardWithReduction you 3) =<< getAvailablePaymentSources you
      unless (null cards) $ chooseOne you
        [targeting c (playCardWithReduction you 3 c) | c <- cards]
      pure e
    _ -> Meditation <$> runMessage msg attrs
