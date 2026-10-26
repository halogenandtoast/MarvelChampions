module Marvel.Event.Events.RapidGrowth (
  rapidGrowth,
  RapidGrowth (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Card.Side
import Marvel.Trait (Trait (Giant))

rapidGrowth :: EventCard RapidGrowth
rapidGrowth = event RapidGrowth Cards.rapidGrowth

newtype RapidGrowth = RapidGrowth (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When you use a basic power: change to your Giant form (side C) and +2 to that power
-- for this use. Already Giant, there is no change and the +2 is still had (owner
-- ruling). The power, waiting on the queue, is the new form's when it resolves.
instance RunMessage RapidGrowth where
  runMessage msg e@(RapidGrowth attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      giant <- identityMatches (IdentityWithTrait Giant) you
      unless giant $ changeToForm you C
      addToBasicPower you 2
      pure e
    _ -> RapidGrowth <$> runMessage msg attrs
