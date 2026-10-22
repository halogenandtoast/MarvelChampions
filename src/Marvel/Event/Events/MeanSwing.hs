module Marvel.Event.Events.MeanSwing (
  meanSwing,
  MeanSwing (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Trait

meanSwing :: EventCard MeanSwing
meanSwing = event MeanSwing Cards.meanSwing

newtype MeanSwing = MeanSwing (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | As the hero makes a basic attack: exhausting a Weapon upgrade is its cost → +3 ATK
-- for it.
instance RunMessage MeanSwing where
  runMessage msg e@(MeanSwing attrs) = case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      addToBasicPower identityId 3
      pure e
    _ -> MeanSwing <$> runMessage msg attrs
