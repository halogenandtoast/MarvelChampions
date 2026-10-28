module Marvel.Event.Events.CosmicAwareness (
  cosmicAwareness,
  CosmicAwareness (..),
) where

import Marvel.Prelude

import Marvel.Aspect
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Ref

cosmicAwareness :: EventCard CosmicAwareness
cosmicAwareness = event CosmicAwareness Cards.cosmicAwareness

newtype CosmicAwareness = CosmicAwareness (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Remove 3 threat from a scheme and discard up to 4 cards from the top of your deck
-- (its cost) → 1 additional threat for each different aspect discarded.
instance RunMessage CosmicAwareness where
  runMessage msg e@(CosmicAwareness attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent you payment _ -> do
        let x = distinctAspects $ map (cdAspect . getCardDef) (paymentDiscards payment)
        removeThreat you attrs (3 + x) ThwartableScheme
        pure e
      _ -> CosmicAwareness <$> runMessage msg attrs
    _ -> CosmicAwareness <$> runMessage msg attrs
