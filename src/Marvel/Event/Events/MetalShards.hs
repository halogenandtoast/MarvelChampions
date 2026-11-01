module Marvel.Event.Events.MetalShards where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.Identity.Types ()

import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

metalShards :: EventCard MetalShards
metalShards = event MetalShards Cards.metalShards

newtype MetalShards = MetalShards (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage MetalShards where
  runMessage msg e@(MetalShards attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) $ dealDamageThen enemy attrs (toDamage 7 $ FromPlayerAttack you) (cardSpecific attrs "attacked" enemy) | enemy <- enemies]
      pure e
    VillainMessage vid VillainDefeated -> do
      insteadOf (\case CardSpecific (isTarget attrs -> True) "attacked" (fromJSON -> Success enemy) -> enemy == EnemyVillainId vid; _ -> False) (becomeTough $ eventController attrs)
      pure e
    MinionMessage mid MinionDefeated -> do
      insteadOf (\case CardSpecific (isTarget attrs -> True) "attacked" (fromJSON -> Success enemy) -> enemy == EnemyMinionId mid; _ -> False) (becomeTough $ eventController attrs)
      pure e
    _ -> MetalShards <$> runMessage msg attrs
