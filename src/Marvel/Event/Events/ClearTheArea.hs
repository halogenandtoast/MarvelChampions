module Marvel.Event.Events.ClearTheArea (
  clearTheArea,
  ClearTheArea (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Count
import Marvel.Ref

clearTheArea :: EventCard ClearTheArea
clearTheArea = event ClearTheArea Cards.clearTheArea

newtype ClearTheArea = ClearTheArea (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Remove 2 threat from a scheme. If this removes the last threat on that scheme, draw 1 card.
instance RunMessage ClearTheArea where
  runMessage msg e@(ClearTheArea attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      schemes <- selectList ThwartableScheme
      chooseOne identityId
        [ targeting (SchemeRef s) do
            thwartScheme s attrs 2
            cardSpecific attrs "check" s
        | s <- schemes
        ]
      pure e
    CardSpecific (isTarget attrs -> True) "check" (fromJSON -> Success s) -> do
      threat <- selectCount SchemeThreat (SchemeWithId s)
      when (threat == 0) $ drawCards (eventController attrs) 1
      pure e
    _ -> ClearTheArea <$> runMessage msg attrs
