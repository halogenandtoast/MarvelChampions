module Marvel.Event.Events.AllForOne (
  allForOne,
  AllForOne (..),
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
import Marvel.Trait (Trait (Avenger))

allForOne :: EventCard AllForOne
allForOne = event AllForOne Cards.allForOne

newtype AllForOne = AllForOne (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (attack): Deal 3 damage to an enemy and exhaust any number of Avenger
-- characters you control. Deal 1 additional damage to that enemy for each character
-- exhausted this way. The enemy is chosen, then the characters one at a time (your
-- hero among them, by the owner's ruling), then the damage is dealt as one attack.
instance RunMessage AllForOne where
  runMessage msg e@(AllForOne attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you
        [targeting (enemyRef enemy) $ cardSpecific attrs "exhaust" (enemy, 0 :: Natural) | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "exhaust" (fromJSON -> Success (enemy :: EnemyId, n :: Natural)) -> do
      let you = eventController attrs
      hero <- selectList (IdentityWithId you <> UnexhaustedIdentity <> IdentityWithTrait Avenger)
      allies <- selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId you) <> AllyWithTrait Avenger)
      let again = cardSpecific attrs "exhaust" (enemy, n + 1)
      chooseOrRunOne you $
        labeled "$label.done" (dealDamage enemy attrs (toDamage (3 + n) $ FromPlayerAttack you))
          : [targeting i (exhaust i >> again) | i <- hero]
          <> [targeting a (exhaust a >> again) | a <- allies]
      pure e
    _ -> AllForOne <$> runMessage msg attrs
