module Marvel.Event.Events.CableArrow (
  cableArrow,
  CableArrow (..),
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

cableArrow :: EventCard CableArrow
cableArrow = event CableArrow Cards.cableArrow

newtype CableArrow = CableArrow (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Remove 3 threat from a scheme, ignoring any crisis icons in play.
instance RunMessage CableArrow where
  runMessage msg e@(CableArrow attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      removeThreat identityId attrs 3 ThwartableSchemeIgnoringCrisis
      pure e
    _ -> CableArrow <$> runMessage msg attrs
