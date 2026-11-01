module Marvel.Attachment.Attachments.HeldHostage (
  heldHostage,
  HeldHostage (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Window ()
import Marvel.Window qualified as W ()

heldHostage :: AttachmentCard HeldHostage
heldHostage = attachment HeldHostage Cards.heldHostage

newtype HeldHostage = HeldHostage (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Attach to the active villain's side scheme. Threat cannot be removed from the
-- attached scheme by thwarting: a basic thwart or an ability labeled a thwart (RR
-- "Thwart"); other cards' "remove threat" still removes it.
instance HasModifiersFor HeldHostage where
  getModifiersFor _ (SideSchemeRef sid) (HeldHostage a) | attachmentSideScheme a == Just sid = pure [ThreatCannotBeRemovedByThwarting]
  getModifiersFor _ _ _ = pure []

-- | Hero Action: the villain corresponding to the attached side scheme attacks you. Then
-- discard this card.
instance HasAbilities HeldHostage where
  getAbilities (HeldHostage a) = case attachmentSideScheme a of
    Just _ -> [ability a 1 HeroAction NoCriteria NoCost (runAbility a 1)]
    Nothing -> []

instance RunMessage HeldHostage where
  runMessage msg h@(HeldHostage attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      selectOne (SignatureSchemeOf ActiveVillain) >>= \case
        Just sid -> attachToSideScheme ident sid
        Nothing -> removeFromPlay attrs
      pure h
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      for_ (attachmentSideScheme attrs) \sid ->
        traverse_ (`enemyAttacks` you) =<< signatureSchemeOwner sid
      removeFromPlay attrs
      pure h
    _ -> HeldHostage <$> runMessage msg attrs
