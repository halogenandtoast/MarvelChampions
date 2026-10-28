module Marvel.Event.Events.MassAttack (
  massAttack,
  MassAttack (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Count
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Ref

massAttack :: EventCard MassAttack
massAttack = event MassAttack Cards.massAttack

newtype MassAttack = MassAttack (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (attack): Exhaust 3 allies you control that share a Trait with your hero
-- (the cost, in the def) → deal X damage to an enemy, X the total ATK of those allies
-- and your hero, as modified: one attack.
instance RunMessage MassAttack where
  runMessage msg e@(MassAttack attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      allies <- for (paymentExhaustedAllies payment) (selectCount AllyAttackValue . AllyWithId)
      hero <- selectCount HeroAttackDamage (IdentityWithId you)
      chooseDamage you attrs (toDamage (sum allies + hero) $ FromPlayerAttack you) AttackableEnemy
      pure e
    _ -> MassAttack <$> runMessage msg attrs
