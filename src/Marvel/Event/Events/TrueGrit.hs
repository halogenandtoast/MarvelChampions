module Marvel.Event.Events.TrueGrit (
  trueGrit,
  TrueGrit (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Count
import Marvel.Ref

trueGrit :: EventCard TrueGrit
trueGrit = event TrueGrit Cards.trueGrit

newtype TrueGrit = TrueGrit (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | After your hero defends against an enemy attack, remove threat from a scheme equal
-- to your hero's THW.
instance RunMessage TrueGrit where
  runMessage msg e@(TrueGrit attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      thw <- selectCount HeroThwartValue (IdentityWithId you)
      removeThreat you attrs thw ThwartableScheme
      pure e
    _ -> TrueGrit <$> runMessage msg attrs
