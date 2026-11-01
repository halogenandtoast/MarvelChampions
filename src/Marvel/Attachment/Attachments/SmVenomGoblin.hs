module Marvel.Attachment.Attachments.SmVenomGoblin where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Resource
import Marvel.Id
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Query
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Hp ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats ()
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types

newtype SmVenomGoblinAttachment = SmVenomGoblinAttachment (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smWeAreOne :: AttachmentCard SmVenomGoblinAttachment
smWeAreOne = attachment SmVenomGoblinAttachment Cards.smWeAreOne

instance HasModifiersFor SmVenomGoblinAttachment where
  getModifiersFor _ (VillainRef vid) (SmVenomGoblinAttachment attrs)
    | attachmentEnemy attrs == Just (EnemyVillainId vid) = pure [AttackModifier 3, SchemeModifier 3]
  getModifiersFor _ _ _ = pure []

instance HasAbilities SmVenomGoblinAttachment where
  getAbilities (SmVenomGoblinAttachment attrs)
    | isJust (attachmentEnemy attrs) = [ability attrs 1 HeroAction NoCriteria (PrintedResourceCost [Energy, Mental, Physical]) $ runAbility attrs 1]
    | otherwise = []

instance RunMessage SmVenomGoblinAttachment where
  runMessage msg e@(SmVenomGoblinAttachment attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> e <$ removeFromPlay attrs
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId attrs -> do
      selectOne ActiveVillain >>= \case
        Nothing -> pure e
        Just vid -> do
          attachedTo vid aid
          pure $ SmVenomGoblinAttachment attrs {attachmentEnemy = Just (EnemyVillainId vid)}
    _ -> SmVenomGoblinAttachment <$> runMessage msg attrs
