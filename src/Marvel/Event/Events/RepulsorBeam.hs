module Marvel.Event.Events.RepulsorBeam (repulsorBeam, RepulsorBeam (..)) where

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

repulsorBeam :: EventCard RepulsorBeam
repulsorBeam = event RepulsorBeam Cards.repulsorBeam

newtype RepulsorBeam = RepulsorBeam (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

beamTargets :: (HasGame m) => Attrs Event -> m [EnemyId]
beamTargets attrs = filterM (fmap not . (\enemy -> cannotTakeDamageFrom (enemyRef enemy) (toSource attrs)))
  =<< selectList (AttackableBy $ IdentityWithId $ eventController attrs)

instance IsEvent RepulsorBeam where
  eventTargets (RepulsorBeam attrs) = do
    stunned <- selectAny (IdentityWithId (eventController attrs) <> StunnedIdentity)
    enemies <- beamTargets attrs
    pure $ if stunned || notNull enemies then Nothing else Just []

instance RunMessage RepulsorBeam where
  runMessage msg e@(RepulsorBeam attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      enemies <- beamTargets attrs
      unless (null enemies) $ chooseOne you
        [targeting (enemyRef enemy) (dealDamage enemy attrs $ toDamage 4 $ FromPlayerAttack you) | enemy <- enemies]
      pure e
    _ -> RepulsorBeam <$> runMessage msg attrs
