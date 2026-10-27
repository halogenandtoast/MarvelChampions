module Marvel.Event.Events.ForwardMomentum (
  forwardMomentum,
  ForwardMomentum (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def (CardMatcher (..), CardType (..), cardMatch)
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Trait.Types qualified as Trait

forwardMomentum :: EventCard ForwardMomentum
forwardMomentum = event ForwardMomentum Cards.forwardMomentum

newtype ForwardMomentum = ForwardMomentum (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Remove 3 threat from a scheme, 5 if you have played an attack event this turn.
instance RunMessage ForwardMomentum where
  runMessage msg e@(ForwardMomentum attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        attacked <-
          any (\(i, def) -> i == identityId && cardMatch (CardWithType EventType <> CardWithTrait Trait.Attack) def)
            <$> getPlayedThisTurn
        removeThreat identityId attrs (if attacked then 5 else 3) ThwartableScheme
        pure e
      _ -> ForwardMomentum <$> runMessage msg attrs
    _ -> ForwardMomentum <$> runMessage msg attrs
