module Marvel.Event.Events.WallcrawlSilk where
import Marvel.Prelude
import Marvel.Query
import Marvel.Id
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers

import Marvel.Ability ()
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Event.Types

wallcrawlSilk :: EventCard WallcrawlSilk
wallcrawlSilk = event WallcrawlSilk Cards.wallcrawlSilk
newtype WallcrawlSilk = WallcrawlSilk (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage WallcrawlSilk where
  runMessage msg u@(WallcrawlSilk attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      removeThreat you attrs 2 ThwartableScheme
      cardSpecific attrs "second" you
      pure u
    CardSpecific (isTarget attrs -> True) "second" (fromJSON -> Success (you :: IdentityId)) -> do
      schemes <- selectList ThwartableScheme
      chooseOne you [targeting (schemeRef scheme) (cardSpecific attrs "scheme" (you,scheme)) | scheme <- schemes]
      pure u
    CardSpecific (isTarget attrs -> True) "scheme" (fromJSON -> Success (you,scheme :: SchemeId)) -> do
      cards <- matchingTuckedCards you (schemeRef scheme)
      chooseOne you (labeled "$label.done" (pure ()) : [targeting card (discardTuckedEncounterCard you card >> thwartSchemeAs you scheme attrs 3) | card <- cards])
      pure u
    _ -> WallcrawlSilk <$> runMessage msg attrs
