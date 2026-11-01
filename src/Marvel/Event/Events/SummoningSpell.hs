module Marvel.Event.Events.SummoningSpell (
  summoningSpell,
  SummoningSpell (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

summoningSpell :: EventCard SummoningSpell
summoningSpell = event SummoningSpell Cards.summoningSpell

newtype SummoningSpell = SummoningSpell (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Discard cards from the top of your deck until you discard an ally → put that ally
-- into play under your control (not played). With none, nothing comes into play.
instance RunMessage SummoningSpell where
  runMessage msg e@(SummoningSpell attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      discardFromDeckUntil you (CardWithType AllyType) attrs
      pure e
    WithDiscardedMatch (isTarget attrs -> True) _ (PlayerCard card) -> do
      putIntoPlay (eventController attrs) card
      pure e
    _ -> SummoningSpell <$> runMessage msg attrs
