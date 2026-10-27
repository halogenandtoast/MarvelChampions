module Marvel.Event.Events.Subdue (
  subdue,
  Subdue (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Window qualified as W
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

subdue :: EventCard Subdue
subdue = event Subdue Cards.subdue

newtype Subdue = Subdue (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Interrupt: When an enemy initiates an attack, that enemy gets -3 ATK for that
-- attack.
instance RunMessage Subdue where
  runMessage msg e@(Subdue attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent _ _ (Just (W.EnemyAttack enemy _))) | ident == eventId attrs -> do
      changeAttack enemy (-3)
      pure e
    _ -> Subdue <$> runMessage msg attrs
