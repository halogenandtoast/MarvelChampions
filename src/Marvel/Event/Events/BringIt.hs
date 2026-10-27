module Marvel.Event.Events.BringIt (
  bringIt,
  BringIt (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Query
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

bringIt :: EventCard BringIt
bringIt = event BringIt Cards.bringIt

newtype BringIt = BringIt (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Draw 1 card for each minion engaged with you.
instance RunMessage BringIt where
  runMessage msg e@(BringIt attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      n <- selectListCount $ MinionEngagedWith (IdentityWithId you)
      when (n > 0) $ drawCards you n
      pure e
    _ -> BringIt <$> runMessage msg attrs
