module Marvel.Attachment.Attachments.RazorClaws where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Id
import Marvel.Keyword (Keyword (Piercing))
import Marvel.Matchers
import Marvel.Query

razorClaws :: AttachmentCard RazorClaws
razorClaws = attachment RazorClaws Cards.razorClaws

newtype RazorClaws = RazorClaws (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor RazorClaws where
  getModifiersFor _ (MinionRef mid) (RazorClaws attrs)
    | attachmentEnemy attrs == Just (EnemyMinionId mid) = pure [AttackModifier 2, KeywordModifier Piercing]
  getModifiersFor _ _ _ = pure []

instance RunMessage RazorClaws where
  runMessage msg a@(RazorClaws attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      minions <- selectList MinionWithHighestPrintedHitPoints
      if null minions
        then removeFromPlay attrs >> surge you
        else chooseOrRunOne you [targeting mid (attachTo aid mid) | mid <- minions]
      pure a
    _ -> RazorClaws <$> runMessage msg attrs
