module Marvel.Event.Events.FirstHit (
  firstHit,
  FirstHit (..),
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
import Marvel.Ref
import Marvel.Window.Types (WindowType (EnemyAttack))

firstHit :: EventCard FirstHit
firstHit = event FirstHit Cards.firstHit

newtype FirstHit = FirstHit (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: deal 2 damage to the villain. Hero Interrupt, when a minion initiates
-- an attack: deal 2 damage to that minion. Which it is, the window it was played in
-- says: none for the action.
instance RunMessage FirstHit where
  runMessage msg e@(FirstHit attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ window) | ident == eventId attrs -> do
      let damage = toDamage 2 $ FromPlayerAttack you
      case window of
        Just (EnemyAttack minion _) -> dealDamage minion attrs damage
        _ -> chooseDamage you attrs damage (VillainEnemy <> AttackableEnemy)
      pure e
    _ -> FirstHit <$> runMessage msg attrs
