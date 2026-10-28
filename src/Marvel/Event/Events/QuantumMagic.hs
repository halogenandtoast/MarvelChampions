module Marvel.Event.Events.QuantumMagic (
  quantumMagic,
  QuantumMagic (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

quantumMagic :: EventCard QuantumMagic
quantumMagic = event QuantumMagic Cards.quantumMagic

newtype QuantumMagic = QuantumMagic (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Action: Choose a card in your discard pile → add that card to your hand.
instance RunMessage QuantumMagic where
  runMessage msg e@(QuantumMagic attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent you _ _ -> do
        chooseFromDiscard you attrs AnyCards 1 1
        pure e
      _ -> QuantumMagic <$> runMessage msg attrs
    WithChosen (isTarget attrs -> True) _ (onlyPlayerCards -> cards) -> do
      for_ cards (addToHand (eventController attrs))
      pure e
    _ -> QuantumMagic <$> runMessage msg attrs
