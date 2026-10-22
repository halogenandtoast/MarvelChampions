module Marvel.Event.Events.DefenderOfTheNineRealms (
  defenderOfTheNineRealms,
  DefenderOfTheNineRealms (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Choice
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref

defenderOfTheNineRealms :: EventCard DefenderOfTheNineRealms
defenderOfTheNineRealms = event DefenderOfTheNineRealms Cards.defenderOfTheNineRealms

newtype DefenderOfTheNineRealms = DefenderOfTheNineRealms (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Discarding until a minion and putting it into play engaged with you is the cost.
-- The minion is put into play, not revealed. A deck emptied with no minion found is a
-- cost fulfilled (RR, Discard): the threat is still removed.
instance RunMessage DefenderOfTheNineRealms where
  runMessage msg e@(DefenderOfTheNineRealms attrs) = case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      pushChoice identityId $ RemoveThreat (toSource attrs) 3 ThwartableScheme
      pure e
    _ -> DefenderOfTheNineRealms <$> runMessage msg attrs
