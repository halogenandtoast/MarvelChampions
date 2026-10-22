module Marvel.Event.Events.Inconspicuous (
  inconspicuous,
  Inconspicuous (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

inconspicuous :: EventCard Inconspicuous
inconspicuous = event Inconspicuous Cards.inconspicuous

newtype Inconspicuous = Inconspicuous (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Remove a total of 3 threat from among schemes in play: 1 at a time, from any scheme.
instance RunMessage Inconspicuous where
  runMessage msg e@(Inconspicuous attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      cardSpecific attrs "remove" (3 :: Natural)
      pure e
    CardSpecific (isTarget attrs -> True) "remove" (fromJSON -> Success (n :: Natural)) | n > 0 -> do
      let you = eventController attrs
      schemes <- selectList ThwartableScheme
      chooseOne you [targeting (SchemeRef s) (thwartScheme s attrs 1 >> cardSpecific attrs "remove" (n - 1)) | s <- schemes]
      pure e
    _ -> Inconspicuous <$> runMessage msg attrs
