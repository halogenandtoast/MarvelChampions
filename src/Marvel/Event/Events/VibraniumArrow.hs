module Marvel.Event.Events.VibraniumArrow (
  vibraniumArrow,
  VibraniumArrow (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Event.Arrow

vibraniumArrow :: EventCard VibraniumArrow
vibraniumArrow = event VibraniumArrow Cards.vibraniumArrow

newtype VibraniumArrow = VibraniumArrow (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 6 damage to an enemy. This attack gains piercing.
instance RunMessage VibraniumArrow where
  runMessage msg e@(VibraniumArrow attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      dmg <- withPiercing <$> arrowDamage identityId 6
      chooseDamage identityId attrs dmg AttackableEnemy
      pure e
    _ -> VibraniumArrow <$> runMessage msg attrs
