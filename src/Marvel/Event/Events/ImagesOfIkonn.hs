module Marvel.Event.Events.ImagesOfIkonn (
  imagesOfIkonn,
  ImagesOfIkonn (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Invocation
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref

imagesOfIkonn :: EventCard ImagesOfIkonn
imagesOfIkonn = event ImagesOfIkonn Cards.imagesOfIkonn

newtype ImagesOfIkonn = ImagesOfIkonn (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Special: confuse the villain and remove 4 threat from a scheme, as much of it as can
-- be done.
instance RunMessage ImagesOfIkonn where
  runMessage msg e@(ImagesOfIkonn attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        villain <- selectJust ActiveVillain
        confuse <- choiceMessages identityId $ Confuse (toRef villain) (toSource attrs)
        threat <- choiceMessages identityId $ RemoveThreat (toSource attrs) 4 ThwartableScheme
        pushAll $ confuse <> threat
        pure e
      ResolvedEvent -> e <$ push (invocationResolved attrs)
    _ -> ImagesOfIkonn <$> runMessage msg attrs
