module Marvel.Event.Events.IAmGroot (
  iAmGroot,
  IAmGroot (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Identity.Types (Field (PlayerIdentityCounters))
import Marvel.Matchers
import Marvel.Projection

iAmGroot :: EventCard IAmGroot
iAmGroot = event IAmGroot Cards.iAmGroot

newtype IAmGroot = IAmGroot (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (thwart): remove threat from a scheme equal to Groot's growth counters.
instance RunMessage IAmGroot where
  runMessage msg e@(IAmGroot attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      n <- project PlayerIdentityCounters you
      removeThreat you attrs n ThwartableScheme
      pure e
    _ -> IAmGroot <$> runMessage msg attrs
