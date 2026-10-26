module Marvel.Event.Events.Perseverance (
  perseverance,
  Perseverance (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

perseverance :: EventCard Perseverance
perseverance = event Perseverance Cards.perseverance

newtype Perseverance = Perseverance (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Response: After you change form, give your hero a tough status card.
instance RunMessage Perseverance where
  runMessage msg e@(Perseverance attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      becomeTough you
      pure e
    _ -> Perseverance <$> runMessage msg attrs
