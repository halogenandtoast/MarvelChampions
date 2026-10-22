module Marvel.Attachment.Attachments.ExoSuit (
  exoSuit,
  ExoSuit (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

exoSuit :: AttachmentCard ExoSuit
exoSuit = attachment ExoSuit Cards.exoSuit

newtype ExoSuit = ExoSuit (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | +1 ATK and +1 SCH to the villain.
instance HasModifiersFor ExoSuit where
  getModifiersFor _ (VillainRef vid) (ExoSuit a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [AttackModifier 1, SchemeModifier 1]
  getModifiersFor _ _ _ = pure []

-- | Hero Action: spend [energy][mental][physical] resources → discard this card.
instance HasAbilities ExoSuit where
  getAbilities (ExoSuit a) = case attachmentEnemy a of
    Just _ ->
      [ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Energy, Just Mental, Just Physical]) $ DiscardTarget (toRef a)]
    Nothing -> []

instance RunMessage ExoSuit where
  runMessage msg (ExoSuit attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villainId <- selectJust ActiveVillain
      attachedTo villainId ident
      pure . ExoSuit $ attrs & enemyL ?~ EnemyVillainId villainId
    _ -> ExoSuit <$> runMessage msg attrs
