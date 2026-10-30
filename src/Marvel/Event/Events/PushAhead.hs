module Marvel.Event.Events.PushAhead where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity.Types (Field (PlayerIdentitySide, PlayerIdentitySides), PlayerIdentitySide (..))
import Marvel.Projection
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

pushAhead :: EventCard PushAhead
pushAhead = event PushAhead Cards.pushAhead

newtype PushAhead = PushAhead (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PushAhead where
  runMessage msg a@(PushAhead attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      side <- project PlayerIdentitySide you
      sides <- project PlayerIdentitySides you
      total <- case lookup side sides of
        Just (HeroSide hero) -> sum <$> sequence [Hero.getModifiedThwart (toAttrs hero), Hero.getModifiedAttack (toAttrs hero), Hero.getModifiedDefense (toAttrs hero)]
        _ -> pure 0
      removeThreat you attrs total SchemeYouCanThwart
      pure a
    _ -> PushAhead <$> runMessage msg attrs
