module Marvel.Attachment.Attachments.RedSkullsRightHook (
  redSkullsRightHook,
  RedSkullsRightHook (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Criteria
import Marvel.Cost
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

redSkullsRightHook :: AttachmentCard RedSkullsRightHook
redSkullsRightHook = attachment RedSkullsRightHook Cards.redSkullsRightHook

newtype RedSkullsRightHook = RedSkullsRightHook (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor RedSkullsRightHook where
  getModifiersFor _ (VillainRef vid) (RedSkullsRightHook a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [KeywordModifier (Retaliate 1)]
  getModifiersFor _ _ _ = pure []

instance HasAbilities RedSkullsRightHook where
  getAbilities (RedSkullsRightHook a) = case attachmentEnemy a of
    Just _ ->
      [ ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Energy, Just Mental, Just Physical]) $
          DiscardTarget (toRef a)
      ]
    _ -> []

instance RunMessage RedSkullsRightHook where
  runMessage msg a@(RedSkullsRightHook attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      attachToTheVillain attrs
      pure a
    _ -> RedSkullsRightHook <$> runMessage msg attrs
