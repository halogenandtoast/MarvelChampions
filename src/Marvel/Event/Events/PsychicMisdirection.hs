module Marvel.Event.Events.PsychicMisdirection where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

psychicMisdirection :: EventCard PsychicMisdirection
psychicMisdirection = event PsychicMisdirection Cards.psychicMisdirection

newtype PsychicMisdirection = PsychicMisdirection (Attrs Event)
  deriving anyclass (HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent PsychicMisdirection where
  eventTargets _ = do
    enemies <- selectList AnyEnemy
    pure $ if length enemies >= 2 then Nothing else Just []

instance RunMessage PsychicMisdirection where
  runMessage msg e@(PsychicMisdirection attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.EnemyAttack enemy _))) | eid == eventId attrs -> do
      enemies <- filter (/= enemy) <$> selectList AnyEnemy
      unless (null enemies) $ chooseOne you
        [targeting (enemyRef other) (redirectAttackDamage enemy other) | other <- enemies]
      pure e
    _ -> PsychicMisdirection <$> runMessage msg attrs
