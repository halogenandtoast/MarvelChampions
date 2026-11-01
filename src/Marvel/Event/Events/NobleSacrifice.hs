module Marvel.Event.Events.NobleSacrifice where

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
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection ()
import Marvel.Query ()
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
nobleSacrifice :: EventCard NobleSacrifice
nobleSacrifice = event NobleSacrifice Cards.nobleSacrifice

newtype NobleSacrifice = NobleSacrifice (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage NobleSacrifice where
  runMessage msg e@(NobleSacrifice attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId attrs -> do
      for_ [hp | DiscardedAllyStatsPayment hp _ _ <- payments payment] $ heal you
      becomeTough you
      pure e
    _ -> NobleSacrifice <$> runMessage msg attrs
