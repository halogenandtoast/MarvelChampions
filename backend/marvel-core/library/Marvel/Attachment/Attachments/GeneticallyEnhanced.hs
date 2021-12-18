module Marvel.Attachment.Attachments.GeneticallyEnhanced
  ( geneticallyEnhanced
  , GeneticallyEnhanced(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Attrs
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Target

geneticallyEnhanced :: AttachmentCard GeneticallyEnhanced
geneticallyEnhanced = attachment GeneticallyEnhanced Cards.geneticallyEnhanced

newtype GeneticallyEnhanced = GeneticallyEnhanced AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasModifiersFor GeneticallyEnhanced where
  getModifiersFor _ (MinionTarget mid) (GeneticallyEnhanced a)
    | Just (EnemyMinionId mid) == attachmentEnemy a = pure [HitPointModifier 3]
  getModifiersFor _ _ _ = pure []

instance RunMessage GeneticallyEnhanced where
  runMessage msg a@(GeneticallyEnhanced attrs) = case msg of
    AttachmentMessage aid msg' | aid == toId attrs -> case msg' of
      RevealAttachment identityId -> do
        minions <- selectList MinionWithHighestPrintedHitPoints
        chooseOne
          identityId
          [ TargetLabel
              (MinionTarget minionId)
              [ Run
                  [ AttachmentMessage (toId attrs)
                    $ AttachedToEnemy
                    $ EnemyMinionId minionId
                  ]
              ]
          | minionId <- minions
          ]
        pure a
      _ -> GeneticallyEnhanced <$> runMessage msg attrs
    _ -> GeneticallyEnhanced <$> runMessage msg attrs
