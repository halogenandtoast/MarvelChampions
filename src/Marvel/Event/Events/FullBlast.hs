module Marvel.Event.Events.FullBlast where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Window qualified as W ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

fullBlast :: EventCard FullBlast
fullBlast = event FullBlast Cards.fullBlast

newtype FullBlast = FullBlast (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage FullBlast where
  runMessage msg e@(FullBlast attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      cardSpecific you "cyclopsFullBlast" ()
      pure e
    _ -> FullBlast <$> runMessage msg attrs
