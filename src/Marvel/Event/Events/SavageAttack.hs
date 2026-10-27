module Marvel.Event.Events.SavageAttack (
  savageAttack,
  SavageAttack (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Ref
import Marvel.Resource

savageAttack :: EventCard SavageAttack
savageAttack = event SavageAttack Cards.savageAttack

newtype SavageAttack = SavageAttack (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 5 damage to an enemy; paid for using only [energy] resources, the attack gains
-- overkill.
instance RunMessage SavageAttack where
  runMessage msg e@(SavageAttack attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      energy <- paidUsingOnly Energy payment
      let overkill = if energy then withOverkill else id
      chooseDamage you attrs (overkill $ toDamage 5 $ FromPlayerAttack you) AttackableEnemy
      pure e
    _ -> SavageAttack <$> runMessage msg attrs
