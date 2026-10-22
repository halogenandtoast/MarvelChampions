module Marvel.Event.Events.SkilledStrike (
  skilledStrike,
  SkilledStrike (..),
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

skilledStrike :: EventCard SkilledStrike
skilledStrike = event SkilledStrike Cards.skilledStrike

newtype SkilledStrike = SkilledStrike (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When your hero makes a basic attack, it gets +2 ATK for that attack.
instance RunMessage SkilledStrike where
  runMessage msg e@(SkilledStrike attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ (Just (W.MadeBasicAttack _))) | ident == eventId attrs -> do
      addToBasicPower identityId 2
      pure e
    _ -> SkilledStrike <$> runMessage msg attrs
