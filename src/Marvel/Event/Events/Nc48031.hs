module Marvel.Event.Events.Nc48031 where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
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
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait
import Marvel.Window ()
import Marvel.Event.Types
import Marvel.Event.Cards qualified as Cards

nc48031 :: EventCard Nc48031
nc48031 = event Nc48031 Cards.nc48031

newtype Nc48031 = Nc48031 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Nc48031 where
  runMessage msg a@(Nc48031 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      minions <- selectList (NotMinion $ MinionWithTrait Trait.Elite)
      chooseOne you [targeting mid (defeatMinion mid) | mid <- minions]
      pure a
    _ -> Nc48031 <$> runMessage msg attrs
