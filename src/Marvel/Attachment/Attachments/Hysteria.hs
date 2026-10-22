module Marvel.Attachment.Attachments.Hysteria (
  hysteria,
  Hysteria (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

hysteria :: AttachmentCard Hysteria
hysteria = attachment Hysteria Cards.hysteria

newtype Hysteria = Hysteria (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Hysteria where
  getAbilities (Hysteria a) = [ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Mental, Just Mental]) $ DiscardTarget (toRef a) | isJust (attachmentEnemy a)]

-- | Attached to Green Goblin: as he schemes or attacks, he is given 1 more boost card.
instance RunMessage Hysteria where
  runMessage msg a@(Hysteria attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villain <- selectJust ActiveVillain
      attachTo ident villain
      pure a
    -- an attack under way, past a stun
    VillainMessage vid (VillainBeginAttack _) | attachedTo vid -> a <$ dealBoost vid
    -- a scheme under way, past a confusion
    VillainMessage vid VillainBeginScheme | attachedTo vid -> a <$ dealBoost vid
    _ -> Hysteria <$> runMessage msg attrs
   where
    attachedTo vid = attachmentEnemy attrs == Just (EnemyVillainId vid)
