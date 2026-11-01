module Marvel.Attachment.Attachments.Nc48029 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Attachment.Types
import Marvel.Attachment.Cards qualified as Cards

nc48029 :: AttachmentCard Nc48029
nc48029 = attachment Nc48029 Cards.nc48029

newtype Nc48029 = Nc48029 (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Nc48029 where
  getModifiersFor _ target (Nc48029 attrs) = pure [modifier | Just enemy <- [attachmentEnemy attrs], target == enemyRef enemy, modifier <- [AttackModifier 1, KeywordModifier Piercing]]

instance HasAbilities Nc48029 where
  getAbilities a@(Nc48029 attrs) = [limitedWindowAbility a 1 (W.EnemyAttacked W.After (EnemyWithId enemy) You)
    HeroResponse NoCriteria (DiscardRandomHandCardsCost 1) (runAbility a 1) | Just enemy <- [attachmentEnemy attrs]]

instance RunMessage Nc48029 where
  runMessage msg a@(Nc48029 attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId attrs -> do
      azazel <- selectOne $ MinionIs Minions.nc48027
      villain <- selectOne ActiveVillain
      let host = (EnemyMinionId <$> azazel) <|> (EnemyVillainId <$> villain)
      for_ host $ \enemy -> attachedTo enemy aid
      pure $ Nc48029 attrs {attachmentEnemy = host}
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ removeFromPlay attrs
    _ -> Nc48029 <$> runMessage msg attrs
