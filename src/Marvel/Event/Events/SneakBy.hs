module Marvel.Event.Events.SneakBy (
  sneakBy,
  SneakBy (..),
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

sneakBy :: EventCard SneakBy
sneakBy = event SneakBy Cards.sneakBy

newtype SneakBy = SneakBy (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SneakBy where
  runMessage msg e@(SneakBy attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        removeThreat identityId attrs 3 ThwartableScheme
        pure e
      _ -> SneakBy <$> runMessage msg attrs
    _ -> SneakBy <$> runMessage msg attrs
