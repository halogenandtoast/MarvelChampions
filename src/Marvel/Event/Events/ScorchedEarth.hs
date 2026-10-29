module Marvel.Event.Events.ScorchedEarth (scorchedEarth, ScorchedEarth (..)) where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

scorchedEarth :: EventCard ScorchedEarth
scorchedEarth = event ScorchedEarth Cards.scorchedEarth

newtype ScorchedEarth = ScorchedEarth (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

earthTargets :: (HasGame m) => Attrs Event -> m [EnemyId]
earthTargets attrs = filterM (fmap not . (\enemy -> cannotTakeDamageFrom (enemyRef enemy) (toSource attrs)))
  =<< selectList AnyEnemy

instance IsEvent ScorchedEarth where
  eventTargets (ScorchedEarth attrs) = do
    enemies <- earthTargets attrs
    pure $ if null enemies then Just [] else Nothing

instance RunMessage ScorchedEarth where
  runMessage msg e@(ScorchedEarth attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent _ _ _) | ident == eventId attrs -> do
      enemies <- earthTargets attrs
      -- One simultaneous, nonattack effect: guard and retaliate do not apply.
      dealDamageToEach enemies attrs (toDamage 3 FromAbility)
      pure e
    _ -> ScorchedEarth <$> runMessage msg attrs
