module Marvel.Attachment.Attachments.AntiRegenerationRay where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source ()
import Marvel.Id (EnemyId (EnemyMinionId))
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Question
import Marvel.Resource
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types

antiRegenerationRay :: AttachmentCard AntiRegenerationRay
antiRegenerationRay = attachment AntiRegenerationRay Cards.antiRegenerationRay

newtype AntiRegenerationRay = AntiRegenerationRay (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor AntiRegenerationRay where
  getModifiersFor _ target (AntiRegenerationRay attrs) = pure
    [modifier | Just target `elem` [enemyRef <$> attachmentEnemy attrs, IdentityRef <$> attachmentIdentity attrs, AllyRef <$> attachmentAlly attrs],
      modifier <- [AttackModifier 1, AttacksBlankDefender]]

instance HasAbilities AntiRegenerationRay where
  getAbilities (AntiRegenerationRay attrs) =
    [ability attrs 1 HeroAction NoCriteria (MultiResourceCost [Just Energy, Just Mental, Just Physical]) (runAbility attrs 1)]

instance RunMessage AntiRegenerationRay where
  runMessage msg a@(AntiRegenerationRay attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId attrs -> do
      host <- selectOne (MinionIs Minions.dreadpool)
      case host of
        Just mid -> attachTo aid (EnemyMinionId mid)
        Nothing -> selectOne ActiveVillain >>= traverse_ (attachTo aid)
      pure a
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ attachToIdentity (attachmentId attrs) you
    _ -> AntiRegenerationRay <$> runMessage msg attrs
