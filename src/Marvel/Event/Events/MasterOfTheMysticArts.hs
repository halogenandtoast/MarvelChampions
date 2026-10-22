module Marvel.Event.Events.MasterOfTheMysticArts (
  masterOfTheMysticArts,
  MasterOfTheMysticArts (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Invocation
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Ref

masterOfTheMysticArts :: EventCard MasterOfTheMysticArts
masterOfTheMysticArts = event MasterOfTheMysticArts Cards.masterOfTheMysticArts

newtype MasterOfTheMysticArts = MasterOfTheMysticArts (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The printed cost of the top Invocation card is paid with this card's own (Doctor
-- Strange adds it), so here its "Special" resolves and the card goes back on top.
instance RunMessage MasterOfTheMysticArts where
  runMessage msg e@(MasterOfTheMysticArts attrs) = case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      push $ invocation identityId (ResolveTopInvocation True)
      pure e
    _ -> MasterOfTheMysticArts <$> runMessage msg attrs
