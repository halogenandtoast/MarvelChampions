module Marvel.Event.Events.GammaSlam (
  gammaSlam,
  GammaSlam (..),
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
import Marvel.Query
import Marvel.Ref

gammaSlam :: EventCard GammaSlam
gammaSlam = event GammaSlam Cards.gammaSlam

newtype GammaSlam = GammaSlam (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage GammaSlam where
  runMessage msg e@(GammaSlam attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        dmg <- min 15 <$> selectCount SustainedDamage (IdentityWithId identityId)
        enemies <- selectList AttackableEnemy
        chooseOne identityId
          [targeting (enemyRef enemy) $ dealDamage enemy attrs (toDamage dmg $ FromPlayerAttack identityId) | enemy <- enemies]
        pure e
      _ -> GammaSlam <$> runMessage msg attrs
    _ -> GammaSlam <$> runMessage msg attrs
