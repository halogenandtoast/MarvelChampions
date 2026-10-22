module Marvel.Attachment.Attachments.TaskmastersShield (
  taskmastersShield,
  TaskmastersShield (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

taskmastersShield :: AttachmentCard TaskmastersShield
taskmastersShield = attachment TaskmastersShield Cards.taskmastersShield

newtype TaskmastersShield = TaskmastersShield (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Attach to Taskmaster." "Taskmaster gains retaliate 1."
instance HasModifiersFor TaskmastersShield where
  getModifiersFor _ (VillainRef vid) (TaskmastersShield a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [KeywordModifier (Retaliate 1)]
  getModifiersFor _ _ _ = pure []

-- | "Hero Action: Exhaust your hero and spend [mental][physical] resources → discard
-- this card."
instance HasAbilities TaskmastersShield where
  getAbilities (TaskmastersShield a) =
    [ ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Mental, Just Physical] <> ExhaustCost) $
        TargetLabel (toRef a) [DiscardTarget $ toRef a]
    | isJust (attachmentEnemy a)
    ]

instance RunMessage TaskmastersShield where
  runMessage msg a@(TaskmastersShield attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villainId <- selectJust ActiveVillain
      attachedTo villainId ident
      pure . TaskmastersShield $ attrs & enemyL ?~ EnemyVillainId villainId
    _ -> TaskmastersShield <$> runMessage msg attrs
