module Marvel.Event.Events.Clobber (
  clobber,
  Clobber (..),
  returnIfFirstThisRound,
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

clobber :: EventCard Clobber
clobber = event Clobber Cards.clobber

newtype Clobber = Clobber (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 3 damage to an enemy. If this is the first card you have played this round,
-- return this card to your hand.
instance RunMessage Clobber where
  runMessage msg e@(Clobber attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      chooseDamage you attrs (toDamage 3 $ FromPlayerAttack you) AttackableEnemy
      returnIfFirstThisRound attrs
      pure e
    _ -> Clobber <$> runMessage msg attrs

-- | "If this is the first card you have played this round, return this card to your
-- hand": asked as it resolves (the card is already among those played, so it is the
-- player's only one), and back from the discard pile once it has gone there. A stun or
-- confusion that cancels the event cancels this too.
returnIfFirstThisRound :: (ReverseQueue m) => Attrs Event -> m ()
returnIfFirstThisRound attrs = do
  mine <- filter ((== eventController attrs) . fst) <$> getPlayedThisRound
  when (length mine == 1) $
    afterMatching (== EventMessage (eventId attrs) ResolvedEvent) (returnToHand attrs)
