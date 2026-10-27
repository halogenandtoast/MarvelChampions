module Marvel.Attachment.Attachments.DrangsSpear (
  drangsSpear,
  DrangsSpear (..),
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
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

drangsSpear :: AttachmentCard DrangsSpear
drangsSpear = attachment DrangsSpear Cards.drangsSpear

-- | Attach to Drang: the villain, Drang being the scenario's only one.
newtype DrangsSpear = DrangsSpear (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Drang gains stalwart.
instance HasModifiersFor DrangsSpear where
  getModifiersFor _ (VillainRef vid) (DrangsSpear a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [KeywordModifier Stalwart]
  getModifiersFor _ _ _ = pure []

-- | Hero Action: spend [mental][physical][physical] resources → discard this card.
instance HasAbilities DrangsSpear where
  getAbilities (DrangsSpear a) = case attachmentEnemy a of
    Just _ ->
      [ ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Mental, Just Physical, Just Physical]) $
          DiscardTarget (toRef a)
      ]
    _ -> []

instance RunMessage DrangsSpear where
  runMessage msg a@(DrangsSpear attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> a <$ attachToTheVillain attrs
    _ -> DrangsSpear <$> runMessage msg attrs
