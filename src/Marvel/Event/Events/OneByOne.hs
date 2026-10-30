module Marvel.Event.Events.OneByOne where

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
import Marvel.Window qualified as W

oneByOne :: EventCard OneByOne
oneByOne = event OneByOne Cards.oneByOne

newtype OneByOne = OneByOne (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage OneByOne where
  runMessage msg e@(OneByOne attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you
        [ targeting (enemyRef enemy) $
            dealDamageThen enemy attrs (toDamage 2 $ FromPlayerAttack you) (cardSpecific attrs "firstAttackDone" enemy)
        | enemy <- enemies ]
      pure e
    CheckWindows windows -> do
      for_ [enemy | W.Window W.After window <- windows, (enemy, source) <- defeat window, source == toSource attrs] $ \enemy ->
        insteadOf (firstAttack enemy) $ cardSpecific attrs "secondAttack" ()
      OneByOne <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "secondAttack" _ -> do
      chooseDamage (eventController attrs) attrs (toDamage 2 $ FromPlayerAttack $ eventController attrs) AttackableEnemy
      pure e
    _ -> OneByOne <$> runMessage msg attrs
   where
    defeat = \case
      W.DefeatedMinion mid _ source _ -> [(EnemyMinionId mid, source)]
      W.DefeatedVillain vid _ source _ -> [(EnemyVillainId vid, source)]
      _ -> []
    firstAttack enemy = \case
      CardSpecific (isTarget attrs -> True) "firstAttackDone" (fromJSON -> Success (enemy' :: EnemyId)) -> enemy == enemy'
      _ -> False
