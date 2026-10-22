module Marvel.Attachment.Attachments.TaskmastersSword (
  taskmastersSword,
  TaskmastersSword (..),
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

taskmastersSword :: AttachmentCard TaskmastersSword
taskmastersSword = attachment TaskmastersSword Cards.taskmastersSword

newtype TaskmastersSword = TaskmastersSword (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Attach to Taskmaster." ATK +1, and "Taskmaster's attacks gain piercing."
instance HasModifiersFor TaskmastersSword where
  getModifiersFor _ (VillainRef vid) (TaskmastersSword a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [AttackModifier 1, KeywordModifier Piercing]
  getModifiersFor _ _ _ = pure []

-- | "Hero Action: Exhaust your hero and spend [mental][physical] resources → discard
-- this card."
instance HasAbilities TaskmastersSword where
  getAbilities (TaskmastersSword a) =
    [ ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Mental, Just Physical] <> ExhaustCost) $
        TargetLabel (toRef a) [DiscardTarget $ toRef a]
    | isJust (attachmentEnemy a)
    ]

instance RunMessage TaskmastersSword where
  runMessage msg a@(TaskmastersSword attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      -- with no villain in play to attach to, it is discarded
      selectOne ActiveVillain >>= \case
        Nothing -> TaskmastersSword attrs <$ removeFromPlay attrs
        Just villainId -> do
          attachedTo villainId ident
          pure . TaskmastersSword $ attrs & enemyL ?~ EnemyVillainId villainId
    _ -> TaskmastersSword <$> runMessage msg attrs
