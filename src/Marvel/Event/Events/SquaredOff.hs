module Marvel.Event.Events.SquaredOff where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.Identity.Types ()

import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

squaredOff :: EventCard SquaredOff
squaredOff = event SquaredOff Cards.squaredOff

newtype SquaredOff = SquaredOff (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SquaredOff where
  runMessage msg e@(SquaredOff attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      cards <- selectList (InHandOf (IdentityWithId you) $ BasicCardMatches $ CardWithType AllyType)
      chooseOne you [targeting c $ playCardWithReduction you 3 c | c <- cards]
      pure e
    _ -> SquaredOff <$> runMessage msg attrs
