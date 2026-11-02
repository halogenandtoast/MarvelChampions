module Marvel.Event.Events.InvestigativeJournalismSilk where
import Marvel.Prelude
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers ()
import Marvel.Question ()
import Marvel.Window qualified as W
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability ()
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Event.Types

investigativeJournalismSilk :: EventCard InvestigativeJournalismSilk
investigativeJournalismSilk = event InvestigativeJournalismSilk Cards.investigativeJournalismSilk
newtype InvestigativeJournalismSilk = InvestigativeJournalismSilk (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage InvestigativeJournalismSilk where
  runMessage msg u@(InvestigativeJournalismSilk attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _you _ (Just (W.EnemySchemeBegins enemy _))) | eid == eventId attrs -> do
      cancelPendingEnemyScheme enemy
      confuse enemy attrs
      pure u
    _ -> InvestigativeJournalismSilk <$> runMessage msg attrs
