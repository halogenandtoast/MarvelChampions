module Marvel.Event.Events.ChillOut where

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
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers
import Marvel.Event.Types

chillOut :: EventCard ChillOut
chillOut = event ChillOut Cards.chillOut

newtype ChillOut = ChillOut (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ChillOut where
  runMessage msg a@(ChillOut attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      removeThreat you attrs 3 ThwartableScheme
      cardSpecific attrs "frostbite" you
      pure a
    CardSpecific (isTarget attrs -> True) "frostbite" (fromJSON -> Success you) -> a <$ chooseFrostbite you
    _ -> ChillOut <$> runMessage msg attrs
