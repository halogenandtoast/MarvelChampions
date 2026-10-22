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

-- | Discarding until a minion and putting it into play engaged with you is the cost:
-- with no minion found, no threat is removed. The minion is put into play, not revealed.
instance RunMessage DefenderOfTheNineRealms where
  runMessage msg e@(DefenderOfTheNineRealms attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent _ _ _ -> do
        push $ DiscardUntil FromEncounterDeck (CardWithType MinionType) (toTarget attrs)
        pure e
      _ -> DefenderOfTheNineRealms <$> runMessage msg attrs
    WithDiscardedMatch (isTarget attrs -> True) _ (EncounterCard card) -> do
      let you = eventController attrs
      thwart <- choiceMessages you $ RemoveThreat (toSource attrs) 3 ThwartableScheme
      pushAll $ PutEncounterCardIntoPlay you card : thwart
      pure e
    _ -> DefenderOfTheNineRealms <$> runMessage msg attrs
