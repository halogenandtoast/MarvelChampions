module Marvel.Event.Events.Impede (
  impede,
  Impede (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Events.Clobber (returnIfFirstThisRound)
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

impede :: EventCard Impede
impede = event Impede Cards.impede

newtype Impede = Impede (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Remove 3 threat from the main scheme. If this is the first card you have played
-- this round, return this card to your hand.
instance RunMessage Impede where
  runMessage msg e@(Impede attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      removeThreat you attrs 3 MainScheme
      returnIfFirstThisRound attrs
      pure e
    _ -> Impede <$> runMessage msg attrs
