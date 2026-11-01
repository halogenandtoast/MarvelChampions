module Marvel.Event.Events.AthleticConditioning (
  athleticConditioning,
  AthleticConditioning (..),
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
import Marvel.Status ()

athleticConditioning :: EventCard AthleticConditioning
athleticConditioning = event AthleticConditioning Cards.athleticConditioning

newtype AthleticConditioning = AthleticConditioning (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Discard 1 stun or confuse status card from your hero, the player
-- choosing which when the hero has both. With neither, it does nothing.
instance RunMessage AthleticConditioning where
  runMessage msg e@(AthleticConditioning attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      stunned <- identityMatches StunnedIdentity you
      confused <- identityMatches ConfusedIdentity you
      when (stunned || confused) $ chooseOrRunOne you $
        [labeled (statusLabel StunnedStatus) (removeStun you) | stunned]
          <> [labeled (statusLabel ConfusedStatus) (removeConfused you) | confused]
      pure e
    _ -> AthleticConditioning <$> runMessage msg attrs
