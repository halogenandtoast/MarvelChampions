module Marvel.Event.Events.QuickStrike (quickStrike, QuickStrike (..)) where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Id (EnemyId)
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Query

quickStrike :: EventCard QuickStrike
quickStrike = event QuickStrike Cards.quickStrike

newtype QuickStrike = QuickStrike (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

strikeTargets :: HasGame m => Attrs Event -> m [EnemyId]
strikeTargets attrs = do
  let you = eventController attrs
  enemies <- selectList (AttackableBy $ IdentityWithId you)
  filterM (\enemy -> do
    immune <- cannotTakeDamageFrom (enemyRef enemy) (toSource attrs)
    atk <- getIdentityAttackAgainst you enemy
    pure $ not immune && atk > 0) enemies

instance IsEvent QuickStrike where
  eventTargets (QuickStrike attrs) = do
    let you = eventController attrs
    stunned <- identityMatches StunnedIdentity you
    enemies <- strikeTargets attrs
    pure $ if stunned || not (null enemies) then Nothing else Just []

instance RunMessage QuickStrike where
  runMessage msg e@(QuickStrike attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- strikeTargets attrs
      unless (null enemies) $ chooseOne you
        [ targeting (enemyRef enemy) do
            atk <- getIdentityAttackAgainst you enemy
            dealDamage enemy attrs (toDamage atk $ FromPlayerAttack you)
        | enemy <- enemies
        ]
      pure e
    _ -> QuickStrike <$> runMessage msg attrs
