module Marvel.Attachment.Attachments.VibraniumArmor (
  vibraniumArmor,
  VibraniumArmor (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

vibraniumArmor :: AttachmentCard VibraniumArmor
vibraniumArmor = attachment VibraniumArmor Cards.vibraniumArmor

newtype VibraniumArmor = VibraniumArmor (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities VibraniumArmor where
  getAbilities (VibraniumArmor a) =
    [ ability
        a
        1
        HeroAction
        NoCriteria
        (MultiResourceCost [Just Physical, Just Physical] <> ExhaustCost)
        (TargetLabel (toRef a) [DiscardTarget $ toRef a])
    ]

instance RunMessage VibraniumArmor where
  runMessage msg a@(VibraniumArmor attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      -- with no villain in play to attach to, it is discarded
      selectOne ActiveVillain >>= \case
        Nothing -> VibraniumArmor attrs <$ removeFromPlay attrs
        Just villainId -> do
          attachedTo villainId ident
          pure . VibraniumArmor $ attrs & enemyL ?~ EnemyVillainId villainId
    -- Forced Response: after the villain takes damage, give it a tough status card. A
    -- tough villain takes no damage, it only loses the card (the game is read before
    -- this message changes it).
    VillainMessage vid (VillainDamaged _ damage)
      | Just (EnemyVillainId vid) == attachmentEnemy attrs
      , damageAmount damage > 0 -> do
          tough <- villainMatches VillainWithToughStatus vid
          unless tough $ enemyBecomesTough vid
          pure a
    _ -> VibraniumArmor <$> runMessage msg attrs
