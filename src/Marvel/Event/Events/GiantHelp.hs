module Marvel.Event.Events.GiantHelp (
  giantHelp,
  GiantHelp (..),
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
import Marvel.Trait (Trait (Giant))

giantHelp :: EventCard GiantHelp
giantHelp = event GiantHelp Cards.giantHelp

newtype GiantHelp = GiantHelp (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Remove 3 threat from a scheme; in Giant form a total of 4 divided among schemes
-- instead. Each scheme given some is thwarted.
instance RunMessage GiantHelp where
  runMessage msg e@(GiantHelp attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      giant <- identityMatches (IdentityWithTrait Giant) you
      if giant
        then thwartDivided you attrs 4 ThwartableScheme
        else removeThreat you attrs 3 ThwartableScheme
      pure e
    _ -> GiantHelp <$> runMessage msg attrs
