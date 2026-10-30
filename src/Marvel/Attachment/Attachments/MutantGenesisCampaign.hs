module Marvel.Attachment.Attachments.MutantGenesisCampaign where

import Marvel.Prelude
import Marvel.Ability (HasAbilities)
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

mgMagnetosPower :: AttachmentCard MutantGenesisCampaignAttachment
mgMagnetosPower = attachment MutantGenesisCampaignAttachment Cards.mgMagnetosPower

newtype MutantGenesisCampaignAttachment = MutantGenesisCampaignAttachment (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor MutantGenesisCampaignAttachment where
  getModifiersFor _ target (MutantGenesisCampaignAttachment attrs)
    | Just enemy <- attachmentEnemy attrs, target == enemyRef enemy = pure [AttackModifier 1,SchemeModifier 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage MutantGenesisCampaignAttachment where
  runMessage msg a@(MutantGenesisCampaignAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId attrs -> do
      magneto <- villainNamed "Magneto"
      for_ magneto (attachTo aid)
      pure a
    _ -> MutantGenesisCampaignAttachment <$> runMessage msg attrs
