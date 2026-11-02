module Marvel.Event.Events.OntheProwlShuri where
import Marvel.Prelude
import Marvel.Id
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.BlackPantherShuri.Helpers

import Marvel.Ability ()
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Event.Types

ontheProwlShuri :: EventCard OntheProwlShuri
ontheProwlShuri = event OntheProwlShuri Cards.ontheProwlShuri
newtype OntheProwlShuri = OntheProwlShuri (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage OntheProwlShuri where
  runMessage msg u@(OntheProwlShuri attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      removeThreat you attrs 3 ThwartableScheme
      cardSpecific attrs "special" you
      pure u
    CardSpecific (isTarget attrs -> True) "special" (fromJSON -> Success (you :: IdentityId)) -> u <$ choosePantherSpecial you
    _ -> OntheProwlShuri <$> runMessage msg attrs
