module Marvel.Event.Events.FightMeCoward (
  fightMeCoward,
  FightMeCoward (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Matchers
import Marvel.Query

fightMeCoward :: EventCard FightMeCoward
fightMeCoward = event FightMeCoward Cards.fightMeCoward

newtype FightMeCoward = FightMeCoward (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Ready your hero and draw 1 card. The villain attacks you.
instance RunMessage FightMeCoward where
  runMessage msg e@(FightMeCoward attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      ready you
      drawCards you 1
      withTheVillain you (`enemyAttacks` you)
      pure e
    _ -> FightMeCoward <$> runMessage msg attrs
