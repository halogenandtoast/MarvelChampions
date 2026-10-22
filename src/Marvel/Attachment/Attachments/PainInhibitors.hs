module Marvel.Attachment.Attachments.PainInhibitors (
  painInhibitors,
  PainInhibitors (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Attachments.DefensiveProgramming (attachToMostRemaining)
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Id
import Marvel.Keyword
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

painInhibitors :: AttachmentCard PainInhibitors
painInhibitors = attachment PainInhibitors Cards.painInhibitors

newtype PainInhibitors = PainInhibitors (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Attached minion gets +2 hit points and gains retaliate 1.
instance HasModifiersFor PainInhibitors where
  getModifiersFor _ (MinionRef mid) (PainInhibitors a)
    | Just (EnemyMinionId mid) == attachmentEnemy a = pure [HitPointModifier 2, KeywordModifier (Retaliate 1)]
  getModifiersFor _ _ _ = pure []

instance RunMessage PainInhibitors where
  runMessage msg (PainInhibitors attrs) = runQueueT $ PainInhibitors <$> attachToMostRemaining Cards.painInhibitors msg attrs
