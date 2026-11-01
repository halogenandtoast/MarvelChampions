module Marvel.Event.Events.Nc48009 where

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
import Marvel.Id
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
import Marvel.Window ()
import Marvel.Event.Types
import Marvel.Event.Cards qualified as Cards

nc48009 :: EventCard Nc48009
nc48009 = event Nc48009 Cards.nc48009

newtype Nc48009 = Nc48009 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Nc48009 where
  runMessage msg a@(Nc48009 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      schemes <- selectList ThwartableScheme
      chooseOne you [targeting (schemeRef scheme) (thwartSchemeAs you scheme attrs 3 >> cardSpecific attrs "second" (you, scheme)) | scheme <- schemes]
      pure a
    CardSpecific (isTarget attrs -> True) "second" (fromJSON -> Success (you :: IdentityId, first :: SchemeId)) -> do
      cards <- selectList (InHandOf (IdentityWithId you) $ BasicCardMatches $ CardWithTitle "Bamf!")
      schemes <- filter (/= first) <$> selectList ThwartableScheme
      unless (null schemes) $ chooseOne you $ labeled "$label.done" (pure ()) :
        [targeting card (discardCard card >> cardSpecific attrs "secondScheme" (you, first)) | card <- cards]
      pure a
    CardSpecific (isTarget attrs -> True) "secondScheme" (fromJSON -> Success (you :: IdentityId, first :: SchemeId)) -> do
      schemes <- filter (/= first) <$> selectList ThwartableScheme
      chooseOne you [targeting (schemeRef scheme) (thwartSchemeAs you scheme attrs 3) | scheme <- schemes]
      pure a
    _ -> Nc48009 <$> runMessage msg attrs
