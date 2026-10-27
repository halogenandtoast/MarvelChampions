module Marvel.Event.Events.LookingForTrouble (
  lookingForTrouble,
  LookingForTrouble (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

lookingForTrouble :: EventCard LookingForTrouble
lookingForTrouble = event LookingForTrouble Cards.lookingForTrouble

newtype LookingForTrouble = LookingForTrouble (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "→ remove 3 threat from the main scheme." Discarding until a minion and putting it
-- into play engaged with you is the cost
-- (as Defender of the Nine Realms): the threat is removed even when no minion is found.
instance RunMessage LookingForTrouble where
  runMessage msg e@(LookingForTrouble attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      removeThreat identityId attrs 3 MainScheme
      pure e
    _ -> LookingForTrouble <$> runMessage msg attrs
