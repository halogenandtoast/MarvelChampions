module Marvel.Event.Events.LastStand (
  lastStand,
  LastStand (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

lastStand :: EventCard LastStand
lastStand = event LastStand Cards.lastStand

newtype LastStand = LastStand (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Interrupt, when an ally you control attacks (its window): +3 ATK for that
-- attack, and after the attack resolves the ally is discarded (nothing more if it has
-- left play meanwhile).
instance RunMessage LastStand where
  runMessage msg e@(LastStand attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent _ _ (Just (W.AllyAttack ally _))) | ident == eventId attrs -> do
      raiseAllyAttack ally 3
      afterMatching (isLanding ally) (removeFromPlay ally)
      pure e
    _ -> LastStand <$> runMessage msg attrs
   where
    isLanding ally = \case
      AllyMessage ally' (AllyAttackLands _ _) -> ally' == ally
      AllyMessage ally' (AllyEvenAttackLands _ _) -> ally' == ally
      _ -> False
