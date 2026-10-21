module Marvel.Attachment.Attachments.GeneticallyEnhanced (
  geneticallyEnhanced,
  GeneticallyEnhanced (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Choice
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref

geneticallyEnhanced :: AttachmentCard GeneticallyEnhanced
geneticallyEnhanced = attachment GeneticallyEnhanced Cards.geneticallyEnhanced

newtype GeneticallyEnhanced = GeneticallyEnhanced (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance HasModifiersFor GeneticallyEnhanced where
  getModifiersFor _ (MinionRef mid) (GeneticallyEnhanced a)
    | Just (EnemyMinionId mid) == attachmentEnemy a = pure [HitPointModifier 3]
  getModifiersFor _ _ _ = pure []

instance RunMessage GeneticallyEnhanced where
  runMessage msg a@(GeneticallyEnhanced attrs) = case msg of
    AttachmentMessage ident msg' | ident == attachmentId attrs -> case msg' of
      RevealAttachment identityId -> do
        minions <- selectList MinionWithHighestPrintedHitPoints
        -- with no minion it is discarded, and surges
        if null minions
          then pushAll [RemoveFromPlay (toTarget attrs), Surge identityId]
          else
            chooseOrRunOne
              identityId
              [ TargetLabel
                (toRef minionId)
                [ Run
                    [ AttachmentMessage (attachmentId attrs) $
                        AttachedToEnemy $
                          EnemyMinionId minionId
                    ]
                ]
              | minionId <- minions
              ]
        pure a
      _ -> GeneticallyEnhanced <$> runMessage msg attrs
    _ -> GeneticallyEnhanced <$> runMessage msg attrs
