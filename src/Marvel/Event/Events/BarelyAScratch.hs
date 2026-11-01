module Marvel.Event.Events.BarelyAScratch where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Window qualified as W

barelyAScratch :: EventCard BarelyAScratch
barelyAScratch = event BarelyAScratch Cards.barelyAScratch

newtype BarelyAScratch = BarelyAScratch (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BarelyAScratch where
  runMessage msg e@(BarelyAScratch attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ (Just (W.IdentityTakeDamage you _))) | eid == eventId attrs -> do
      amount <- getEncounterIconCount
      preventDamage you amount
      pure e
    EventMessage eid (PlayedEvent _ _ (Just (W.IdentityTakeDamageFrom you _ _))) | eid == eventId attrs -> do
      amount <- getEncounterIconCount
      preventDamage you amount
      pure e
    _ -> BarelyAScratch <$> runMessage msg attrs
