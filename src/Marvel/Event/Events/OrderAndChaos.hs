module Marvel.Event.Events.OrderAndChaos (
  orderAndChaos,
  OrderAndChaos (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W

orderAndChaos :: EventCard OrderAndChaos
orderAndChaos = event OrderAndChaos Cards.orderAndChaos

newtype OrderAndChaos = OrderAndChaos (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When a treachery is revealed from the encounter deck, cancel its "When Revealed"
-- effects (its incite too, RR Incite X), then deal 2 damage to the villain.
instance RunMessage OrderAndChaos where
  runMessage msg e@(OrderAndChaos attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent _ _ (Just (W.RevealTreachery tid W.RevealedFromEncounterDeck))) | ident == eventId attrs -> do
      cancelWhenRevealed tid
      -- with no villain in play (as between one Kang and the next), no damage
      selectOne ActiveVillain >>= traverse_ \villain -> dealDamage villain attrs (toDamage 2 FromAbility)
      pure e
    _ -> OrderAndChaos <$> runMessage msg attrs
