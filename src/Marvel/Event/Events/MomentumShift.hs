module Marvel.Event.Events.MomentumShift (
  momentumShift,
  MomentumShift (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref

momentumShift :: EventCard MomentumShift
momentumShift = event MomentumShift Cards.momentumShift

newtype MomentumShift = MomentumShift (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Healing 2 damage from your hero is its cost → deal 2 damage to an enemy.
instance RunMessage MomentumShift where
  runMessage msg e@(MomentumShift attrs) = case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      pushChoice identityId $
        ChooseDamage (toSource attrs) (toDamage 2 $ FromPlayerAttack identityId) AttackableEnemy
      pure e
    _ -> MomentumShift <$> runMessage msg attrs
