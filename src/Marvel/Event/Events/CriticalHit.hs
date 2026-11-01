module Marvel.Event.Events.CriticalHit where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

criticalHit :: EventCard CriticalHit
criticalHit = event CriticalHit Cards.criticalHit

newtype CriticalHit = CriticalHit (Attrs Event)
  deriving anyclass (HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent CriticalHit where
  eventTargets (CriticalHit attrs) = do
    windows <- getCurrentWindows
    let enemies = concat [targets | W.Window W.After (W.PlayerAttackResolved _ you targets) <- windows, you == eventController attrs]
    eligible <- filterM (selectAny . (<> EnemyCanBeStunned) . EnemyWithId) enemies
    pure $ if null eligible then Just [] else Nothing

instance RunMessage CriticalHit where
  runMessage msg a@(CriticalHit attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.PlayerAttackResolved source attacker enemies))) | eid == eventId attrs -> do
      when (attacker == you && case source of AllyRef _ -> False; _ -> True) do
        eligible <- filterM (selectAny . (<> EnemyCanBeStunned) . EnemyWithId) enemies
        unless (null eligible) $ chooseOrRunOne you [targeting (enemyRef enemy) (stun enemy attrs) | enemy <- eligible]
      pure a
    _ -> CriticalHit <$> runMessage msg attrs
