module Marvel.Attachment.Attachments.GeneTherapy where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card ()
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Stats ()
import Marvel.Window qualified as W
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Keyword

geneTherapy :: AttachmentCard GeneTherapy
geneTherapy = attachment (GeneTherapy . (`With` TherapyMeta False)) Cards.geneTherapy

data TherapyMeta = TherapyMeta {attackPending :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype GeneTherapy = GeneTherapy (Attrs Attachment `With` TherapyMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsAttachment GeneTherapy where
  toAttachmentAttrs (GeneTherapy (attrs `With` _)) = attrs

instance IsRef GeneTherapy where
  toRef = toRef . toAttachmentAttrs

instance HasModifiersFor GeneTherapy where
  getModifiersFor _ target (GeneTherapy (attrs `With` _)) = pure
    [AttackModifier 2 | Just target == (enemyRef <$> attachmentEnemy attrs)]

instance HasAbilities GeneTherapy where
  getAbilities a@(GeneTherapy (attrs `With` _)) = case attachmentEnemy attrs of
    Just enemy ->
      [ windowAbility a 1 (W.EnemyAttacked W.When (EnemyWithId enemy) AnyIdentity) ForcedInterrupt NoCost (runAbility a 1)
      ]
    Nothing -> []

instance RunMessage GeneTherapy where
  runMessage msg e@(GeneTherapy (attrs `With` meta)) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      enemies <- selectList (NotEnemy $ EnemyWithAttachment $ AttachmentIs Cards.geneTherapy)
      ranked <- for enemies (\enemy -> (enemy,) <$> getEnemyPrintedAttack enemy)
      case ranked of
        [] -> removeFromPlay attrs >> surge you
        _ -> do
          let lowest = minimum (map snd ranked)
          chooseOrRunOne you [targeting (enemyRef enemy) (attachTo aid enemy) | (enemy, atk) <- ranked, atk == lowest]
      pure e
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      for_ (attachmentEnemy attrs) $ \enemy -> do
        attackGains enemy Overkill
        attackGains enemy Piercing
      pure $ GeneTherapy (attrs `With` TherapyMeta True)
    MinionMessage mid MinionEndAttack | attackPending meta && attachmentEnemy attrs == Just (EnemyMinionId mid) -> e <$ removeFromPlay attrs
    VillainMessage vid VillainEndAttack | attackPending meta && attachmentEnemy attrs == Just (EnemyVillainId vid) -> e <$ removeFromPlay attrs
    _ -> GeneTherapy . (`With` meta) <$> runMessage msg attrs
