{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Attachment.Attachments.Fw54035 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Attachment.Types

import Marvel.FalconWinter.Helpers
fw54035 :: AttachmentCard Fw54035
fw54035 = attachment Fw54035 Cards.fw54035

newtype Fw54035 = Fw54035 (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54035 where
  getAbilities a@(Fw54035 attrs) =
    []

instance HasModifiersFor Fw54035 where
  getModifiersFor _ target (Fw54035 attrs) | Just target == (characterRef <$> iceHost attrs) = pure [OnlyAttackAttachment $ attachmentId attrs]
  getModifiersFor _ _ _ = pure []

instance RunMessage Fw54035 where
  runMessage msg a@(Fw54035 attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      attachToIdentity aid you
      pure a
    AttachmentMessage aid (AttachmentDamaged amount) | aid == attachmentId attrs -> do
      let total = attachmentDamage attrs + amount
      when (total >= 3) $ do
        for_ (iceHost attrs) $ \host -> moveIceDamage attrs host (total - 3)
        removeFromPlay attrs
      pure $ Fw54035 attrs {attachmentDamage = total}
    _ -> Fw54035 <$> runMessage msg attrs
