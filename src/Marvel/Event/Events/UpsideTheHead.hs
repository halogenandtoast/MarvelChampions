module Marvel.Event.Events.UpsideTheHead where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability.Types qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

upsideTheHead :: EventCard UpsideTheHead
upsideTheHead = event UpsideTheHead Cards.upsideTheHead

newtype UpsideTheHead = UpsideTheHead (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage UpsideTheHead where
  runMessage msg e@(UpsideTheHead a) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.BasicAttackDamaged _ enemies))) | eid == eventId a -> do
      alive <- filterM (selectAny . EnemyWithId) enemies
      chooseOne you [targeting (enemyRef enemy) $ do
        confused <- selectAny (EnemyWithId enemy <> ConfusedEnemy)
        if confused then stun enemy a else confuse enemy a
        | enemy <- alive]
      pure e
    _ -> UpsideTheHead <$> runMessage msg a
