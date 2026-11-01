module Marvel.Event.Events.YouGotThis where

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
import Marvel.Window qualified as W
import Marvel.Identity.Types ()

import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
youGotThis :: EventCard YouGotThis
youGotThis = event YouGotThis Cards.youGotThis

newtype YouGotThis = YouGotThis (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage YouGotThis where
  runMessage msg e@(YouGotThis attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment window) | eid == eventId attrs -> do
      let attack = case window of Just (W.MadeBasicAttack _) -> True; _ -> False
      for_ [if attack then atk else thw | DiscardedAllyStatsPayment _ thw atk <- payments payment] $ addToBasicPower you
      ready you
      pure e
    _ -> YouGotThis <$> runMessage msg attrs
