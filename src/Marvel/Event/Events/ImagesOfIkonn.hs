module Marvel.Event.Events.ImagesOfIkonn (
  imagesOfIkonn,
  ImagesOfIkonn (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Invocation
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref

imagesOfIkonn :: EventCard ImagesOfIkonn
imagesOfIkonn = event ImagesOfIkonn Cards.imagesOfIkonn

newtype ImagesOfIkonn = ImagesOfIkonn (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Special: confuse the villain and remove 4 threat from a scheme, as much of it as can
-- be done.
instance RunMessage ImagesOfIkonn where
  runMessage msg e@(ImagesOfIkonn attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        -- with no villain in play (as between one Kang and the next), nothing
        withTheVillain identityId \villain -> do
          confuse villain attrs
          removeThreat identityId attrs 4 ThwartableScheme
        pure e
      ResolvedEvent -> e <$ invocationResolved attrs
      _ -> ImagesOfIkonn <$> runMessage msg attrs
    _ -> ImagesOfIkonn <$> runMessage msg attrs
