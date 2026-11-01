module Marvel.Event.Events.Nc48011 where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
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
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Event.Types
import Marvel.Event.Cards qualified as Cards

nc48011 :: EventCard Nc48011
nc48011 = event Nc48011 Cards.nc48011

newtype Nc48011 = Nc48011 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Nc48011 where
  runMessage msg a@(Nc48011 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.DefendedWithBamf _ enemy bamf))) | eid == eventId attrs -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ CardWithId $ pcCardId bamf)
      for_ cards (addToHand you)
      dealDamage enemy attrs (toDamage 3 FromAbility)
      pure a
    _ -> Nc48011 <$> runMessage msg attrs
