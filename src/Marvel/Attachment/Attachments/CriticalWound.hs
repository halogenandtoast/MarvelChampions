module Marvel.Attachment.Attachments.CriticalWound where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types

criticalWound :: AttachmentCard CriticalWound
criticalWound = attachment CriticalWound Cards.criticalWound

newtype CriticalWound = CriticalWound (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities CriticalWound where
  getAbilities a@(CriticalWound attrs) =
    [ windowAbility a 1 (W.TurnEnds W.When $ IdentityWithId you) ForcedInterrupt NoCost (runAbility a 1)
    | Just you <- [attachmentIdentity attrs]
    ]

instance RunMessage CriticalWound where
  runMessage msg a@(CriticalWound attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> a <$ attachToIdentity aid you
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      removeFromPlay attrs
      for_ (attachmentIdentity attrs) $ \you -> damageIdentity you attrs (toDamage 4 FromAbility)
      pure a
    _ -> CriticalWound <$> runMessage msg attrs
