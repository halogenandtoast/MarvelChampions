module Marvel.Event.Events.SuperpowerAdaptation where

import Marvel.Id
import Marvel.Prelude
import Data.List (find)
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Aspect
import Marvel.Payment
import Marvel.Identity.Types (PlayerIdentity)
import Marvel.Projection (Projection)
import Marvel.Queue (HasQueue)

superpowerAdaptation :: EventCard SuperpowerAdaptation
superpowerAdaptation = event SuperpowerAdaptation Cards.superpowerAdaptation

newtype SuperpowerAdaptation = SuperpowerAdaptation (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

adaptableEvents :: (HasGame m, HasQueue m, MonadThrow m, Projection m PlayerIdentity) => IdentityId -> m [PlayerCard]
adaptableEvents you = do
  host <- touchedHost you
  case host of
    Just character@(IdentityCharacter _) -> matchingEvents character Nothing
    Just character@(AllyCharacter _) -> do
      card <- getsGame (\g -> cardInPlay g $ characterRef character)
      maybe (pure []) (matchingEvents character . cdAspect . getCardDef) card
    _ -> pure []
 where
  matchingEvents character classification = do
    owner <- getCharacterOwner character
    case owner of
      Nothing -> pure []
      Just p -> do
        cards <- selectList (InDiscardOf (IdentityWithId p) $ BasicCardMatches $ CardWithType EventType)
        pure $ filter (\c -> case classification of
          Nothing -> isNothing (cdAspect $ getCardDef c)
          Just Neutral -> cdAspect (getCardDef c) == Just Neutral
          Just _ -> maybe False (/= Neutral) (cdAspect $ getCardDef c)) cards

instance IsEvent SuperpowerAdaptation where
  eventTargets (SuperpowerAdaptation attrs) = Just . map toTarget <$> adaptableEvents (eventController attrs)

instance RunMessage SuperpowerAdaptation where
  runMessage msg e@(SuperpowerAdaptation attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId attrs -> do
      cards <- adaptableEvents you
      for_ (paymentTarget payment) $ \target ->
        for_ (find ((== target) . toTarget) cards) $ \card -> do
          removeFromGame card
          addToHand you card
      pure e
    _ -> SuperpowerAdaptation <$> runMessage msg attrs
