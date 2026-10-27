module Marvel.Event.Events.EverVigilant (
  everVigilant,
  EverVigilant (..),
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

everVigilant :: EventCard EverVigilant
everVigilant = event EverVigilant Cards.everVigilant

newtype EverVigilant = EverVigilant (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Ready your hero and remove 2 threat from the main scheme (none while a crisis
-- side scheme is in play, the main scheme's doing).
instance RunMessage EverVigilant where
  runMessage msg e@(EverVigilant attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      ready you
      removeThreat you attrs 2 MainScheme
      pure e
    _ -> EverVigilant <$> runMessage msg attrs
