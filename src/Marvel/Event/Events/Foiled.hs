module Marvel.Event.Events.Foiled (
  foiled,
  Foiled (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

foiled :: EventCard Foiled
foiled = event Foiled Cards.foiled

newtype Foiled = Foiled (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When a boost card is turned faceup during a scheme activation, cancel its boost
-- icons: they are added just after it turns faceup, so that is taken off the queue.
instance RunMessage Foiled where
  runMessage msg e@(Foiled attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent _ _ (Just (W.FlippedBoostCard _ (EnemyVillainId vid)))) | ident == eventId attrs -> do
      cancelBoostIcons vid
      pure e
    _ -> Foiled <$> runMessage msg attrs
