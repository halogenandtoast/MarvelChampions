module Marvel.Attachment.Attachments.Unstoppable where

import Marvel.Prelude
import Marvel.Card.Side
import Marvel.Payment
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Stats
import Marvel.Status
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Types qualified as Minion
import Marvel.Villain.Types qualified as Villain
import Marvel.Keyword

unstoppable :: AttachmentCard Unstoppable
unstoppable = attachment Unstoppable Cards.unstoppable

newtype Unstoppable = Unstoppable (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Unstoppable where
  getAbilities (Unstoppable a) =
    case attachmentEnemy a of
      Nothing -> []
      Just enemy ->
        [windowAbility a 1 (W.EnemyAttacked W.When (EnemyWithId enemy) AnyIdentity) ForcedInterrupt NoCost (runAbility a 1)
        ,windowAbility a 2 (W.EnemyAttacked W.After (EnemyWithId enemy) AnyIdentity) ForcedResponse NoCost (runAbility a 2)]

instance RunMessage Unstoppable where
  runMessage msg e@(Unstoppable attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      enemies <- selectList (NotEnemy (EnemyWithAttachment $ AttachmentIs Cards.unstoppable))
      values <- for enemies \enemy -> (enemy,) <$> getEnemyPrintedAttack enemy
      let highest = maximum (0 : map snd values)
      case [enemy | (enemy, n) <- values, n == highest] of
        [] -> surge you >> removeFromPlay attrs
        eligible -> chooseOne you [targeting (enemyRef enemy) (attachTo aid enemy) | enemy <- eligible]
      pure e
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      for_ (attachmentEnemy attrs) \enemy -> do
        attackGains enemy Overkill
        attackGains enemy Piercing
      pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> e <$ removeFromPlay attrs
    _ -> Unstoppable <$> runMessage msg attrs

instance HasModifiersFor Unstoppable where
  getModifiersFor _ (VillainRef vid) (Unstoppable a) = pure [AttackModifier 2 | attachmentEnemy a == Just (EnemyVillainId vid)]
  getModifiersFor _ (MinionRef mid) (Unstoppable a) = pure [AttackModifier 2 | attachmentEnemy a == Just (EnemyMinionId mid)]
  getModifiersFor _ _ _ = pure []
