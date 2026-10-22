module Marvel.Event.Events.SplitPersonality (
  splitPersonality,
  SplitPersonality (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Side
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

splitPersonality :: EventCard SplitPersonality
splitPersonality = event SplitPersonality Cards.splitPersonality

newtype SplitPersonality = SplitPersonality (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SplitPersonality where
  runMessage msg e@(SplitPersonality attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        inHero <- identityMatches HeroIdentity identityId
        changeToForm identityId (if inHero then B else A)
        drawToPrintedHandSize identityId
        pure e
      _ -> SplitPersonality <$> runMessage msg attrs
    _ -> SplitPersonality <$> runMessage msg attrs
