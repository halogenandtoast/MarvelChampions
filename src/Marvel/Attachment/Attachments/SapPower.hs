module Marvel.Attachment.Attachments.SapPower (
  sapPower,
  SapPower (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W

sapPower :: AttachmentCard SapPower
sapPower = attachment SapPower Cards.sapPower

newtype SapPower = SapPower (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Forced Response: after your turn ends, take 1 damage.
-- Alter-Ego Action: spend [energy][energy] resources → discard this card.
instance HasAbilities SapPower where
  getAbilities (SapPower a) = case attachmentIdentity a of
    Just ident ->
      [ windowAbility a 1 (W.TurnEnds W.After (IdentityWithId ident)) ForcedResponse NoCost $ runAbility a 1
      , ability a 2 AlterEgoAction NoCriteria (MultiResourceCost [Just Energy, Just Energy]) $ DiscardTarget (toRef a)
      ]
    Nothing -> []

-- | Attach to your identity.
instance RunMessage SapPower where
  runMessage msg a@(SapPower attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      attachToIdentity ident you
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      for_ (attachmentIdentity attrs) \you -> damageIdentity you attrs (toDamage 1 FromAbility)
      pure a
    _ -> SapPower <$> runMessage msg attrs
