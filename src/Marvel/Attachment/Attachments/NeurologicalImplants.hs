module Marvel.Attachment.Attachments.NeurologicalImplants (
  neurologicalImplants,
  NeurologicalImplants (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Attachments.DefensiveProgramming (attachToMostRemaining)
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

neurologicalImplants :: AttachmentCard NeurologicalImplants
neurologicalImplants = attachment NeurologicalImplants Cards.neurologicalImplants

newtype NeurologicalImplants = NeurologicalImplants (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Its printed +2 ATK and +2 SCH go to the attached minion, which also gets +2 hit
-- points.
instance HasModifiersFor NeurologicalImplants where
  getModifiersFor _ (MinionRef mid) (NeurologicalImplants a)
    | Just (EnemyMinionId mid) == attachmentEnemy a = pure [AttackModifier 2, SchemeModifier 2, HitPointModifier 2]
  getModifiersFor _ _ _ = pure []

instance RunMessage NeurologicalImplants where
  runMessage msg (NeurologicalImplants attrs) = runQueueT $ NeurologicalImplants <$> attachToMostRemaining Cards.neurologicalImplants msg attrs
