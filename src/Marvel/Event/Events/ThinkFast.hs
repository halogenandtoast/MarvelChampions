module Marvel.Event.Events.ThinkFast (
  thinkFast,
  ThinkFast (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Query ()
import Marvel.Matchers ()
import Marvel.Damage
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

thinkFast :: EventCard ThinkFast
thinkFast = event ThinkFast Cards.thinkFast

newtype ThinkFast = ThinkFast (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Take 1 damage. Confuse the villain.
instance RunMessage ThinkFast where
  runMessage msg e@(ThinkFast attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      damageIdentity you attrs (toDamage 1 FromAbility)
      withTheVillain you (`confuse` attrs)
      pure e
    _ -> ThinkFast <$> runMessage msg attrs
