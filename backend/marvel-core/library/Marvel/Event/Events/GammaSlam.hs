module Marvel.Event.Events.GammaSlam
  ( gammaSlam
  , GammaSlam(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Target

gammaSlam :: EventCard GammaSlam
gammaSlam = event GammaSlam Cards.gammaSlam

newtype GammaSlam = GammaSlam EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage GammaSlam where
  runMessage msg e@(GammaSlam attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        dmg <- min 15
          <$> selectCount SustainedDamage (IdentityWithId identityId)
        enemies <- selectList AttackableEnemy
        chooseOne identityId $ map (damageChoice attrs (toDamage dmg $ FromPlayerAttack identityId)) enemies
        pure e
      _ -> GammaSlam <$> runMessage msg attrs
    _ -> GammaSlam <$> runMessage msg attrs
