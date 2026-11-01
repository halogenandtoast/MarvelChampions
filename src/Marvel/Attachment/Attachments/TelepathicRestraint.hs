module Marvel.Attachment.Attachments.TelepathicRestraint where

import Marvel.Prelude
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Id ()
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types

telepathicRestraint :: AttachmentCard TelepathicRestraint
telepathicRestraint = attachment TelepathicRestraint Cards.telepathicRestraint

newtype TelepathicRestraint = TelepathicRestraint (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TelepathicRestraint where
  getAbilities (TelepathicRestraint a) =
    [ability a 1 Action NoCriteria (MultiResourceCost [Just Mental, Just Mental]) (runAbility a 1)]

instance HasModifiersFor TelepathicRestraint where
  getModifiersFor _ (IdentityRef you) (TelepathicRestraint a) = pure [ContinuouslyStunned | attachmentIdentity a == Just you]
  getModifiersFor _ _ _ = pure []

instance RunMessage TelepathicRestraint where
  runMessage msg e@(TelepathicRestraint attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      attachToIdentity aid you
      pure $ TelepathicRestraint attrs {attachmentIdentity = Just you}
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> e <$ removeFromPlay attrs
    _ -> TelepathicRestraint <$> runMessage msg attrs
