module Marvel.Event.Events.SwarmTactics (
  swarmTactics,
  SwarmTactics (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

swarmTactics :: EventCard SwarmTactics
swarmTactics = event SwarmTactics Cards.swarmTactics

newtype SwarmTactics = SwarmTactics (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Change to your other hero form. Ready your hero.
instance RunMessage SwarmTactics where
  runMessage msg e@(SwarmTactics attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      changeToHeroForm you
      ready you
      pure e
    _ -> SwarmTactics <$> runMessage msg attrs
